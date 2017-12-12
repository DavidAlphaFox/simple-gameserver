%% @author caohongyang
%% @doc 武将相关操作协议
%% Created 2013-3-8


-module(role_ger).
-include("def_role.hrl").
-include("def_reward.hrl").
-compile(export_all).
%% API functions
-export([]).

%% Internal functions
-export([]).
-define(MAX_UP_RANK,0).
-define(UP_RANK_ONE,1).

%% ====================================================================
%% API functions
%% ====================================================================
cs_ger_info(_) ->
    PGerBag = [ger_lib:ger2p_ger(E)||E<-role_data:get_gerBag()],
    PPosList = [ger_lib:ger2p_ger(E)||E<-role_data:get_posListT()],
    PLPosList = [ger_lib:ger2p_ger(E)||E<-role_data:get_lieuposList()],
    ?INFO("cs_ger_info~n~w~n~w~n~w",[PGerBag,PPosList,PLPosList]),
    Trainer = #p_ger{gerID =1000,
                     gerTypeID    =1000,
                     gerQuality   =0,    
                     gerLevel     =0,  
                     gerAttack    =0, 
                     gerHpMax     =0,
                     gerFightPower=0,
                     gerExp       =0,
                     gerProMean   =0,
                     gerBody   =0,
                     gerDemageRate =[],
                     gerSpeed = 0
                    },
    
    %需要的话，把魔镜中的精灵显示在背包中
    FirstGetList0 = [Trainer|PPosList++PLPosList],
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_}}  = role_data:get_mirror(),
    FirstGetList = case GerSimpleInfo#gerSimple.gerID of
                       0 ->
                           FirstGetList0;
                       _ ->
                           [ger_lib:ger2p_ger(GerSimpleInfo)|FirstGetList0]
                   end,
    
    %% 	Reply = #sc_ger_info{gerList=PLPosList++PPosList++PGerBag},
    %% 	?sendself(Reply).
    BagLength = erlang:length(PGerBag),
    SeqLength = data_common:get(send_client_pak_cnt),
    SeqCnt = 
        case BagLength of
            0 ->
                1;
            _ ->
                2 + BagLength div SeqLength
        end,
    ?sendself(#sc_ger_info2{seq=SeqCnt+1,subSeq=1,gerList = FirstGetList}),
    lists:foldl(fun(Cnt,Acc)->
                        case Cnt of
                            SeqCnt ->
                                ?sendself(#sc_ger_info2{seq=SeqCnt,subSeq=Cnt,gerList=Acc});
                            _ ->
                                {Head,Tail} = lists:split(SeqLength, Acc),
                                ?sendself(#sc_ger_info2{seq=SeqCnt,subSeq=Cnt,gerList=Head}),
                                Tail
                        end
                end,PGerBag,lists:seq(2,SeqCnt)).


cs_ger_pos_list(_) ->
    PosList = [begin
                   EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],
                   GerDimandInfo = GerHolyGrailInfo#holyGrail.diamondInfo,				   
                   #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList,diamondList=[role_diamond:to_p_diamond(E)||E<-GerDimandInfo]}
               end ||#ger{gerBase=#gerBase{gerPos=Pos,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID}<-role_data:get_posList()],
    {TagID,Data,IconData} = role_data:get_tag_data(),
    #p_ger_tag_dtl{tag=MaxTagID0} 
                      = case lists:keysort(#p_ger_tag_dtl.tag, Data) of [] -> #p_ger_tag_dtl{tag=0};X -> hd(lists:reverse(X)) end,
    MaxTagID1 = erlang:length(Data),
    MaxTagID =  if MaxTagID0 < 2 -> 2; true -> max(MaxTagID0+1,MaxTagID1) end,
	Reply = #sc_ger_pos_list{gerPosInfoList=PosList,useTagID=TagID,maxTagID=MaxTagID,iconList=IconData},
    ?sendself(Reply).

%% 出战一个武将
cs_ger_standup(#cs_ger_standup{gerPos=GerPos, gerID=GerID})->
    case check_standup(GerPos, GerID) of
        {true, UpGer, PosList, GerBag2} ->
            do_standup(GerPos, GerID, UpGer, PosList, GerBag2);
        {false,Reason} ->
            ?sendself(#sc_ger_standup{gerPos=GerPos,gerID=GerID, result=Reason})
    end.

%% 下阵一个武将
cs_ger_unload(#cs_ger_unload{gerPos=GerPos}) ->
    case check_unload(GerPos) of
        {true, UnLoadGer, PosList2} ->
            do_unload(GerPos, UnLoadGer, PosList2);
        {false, Reason} ->
            ?sendself(#sc_ger_unload{result=Reason, gerPos=GerPos})
    end.

unload_ex_ger()->
    PosList = ger_attr:get_original_posList(),
    #role{level=Level} = role_data:get_roleInfo(),
    NowGers = 	length(PosList),
    GerStandLimit = data_stand_fighters:get(Level),
    if NowGers > GerStandLimit ->
           {EState,_} = 
               lists:foldl(fun(#ger{gerBase=#gerBase{gerPos=GerPos}},{State,C}) -> 
                                   if C > 0 -> cs_ger_unload(#cs_ger_unload{gerPos=GerPos}),{true,C-1};
                                      true -> {State,C}
                                   end
                           end, {false,NowGers-GerStandLimit}, PosList),
           if EState ->	[begin ?CATCH(role_task_trigger:handle({dispach_task,role_up_ger_num,N})),
                               ?CATCH_1(role_task_trigger:handle({dispach_task,role_extract_card,N,[20040]}),E2)
                         end||N<-lists:seq(1,GerStandLimit)];
              true -> []
           end;				  
       true ->
           ignore
    end,
    NewPosList = ger_attr:get_original_posList(),
    lists:foldl(fun(#ger{gerBase=#gerBase{gerTypeID=GerTypeID,gerPos=GerPos}},ONGerListAcc)->
                        case lists:member(GerTypeID, ONGerListAcc) of
                            true -> cs_ger_unload(#cs_ger_unload{gerPos=GerPos}),ONGerListAcc;
                            _ ->[GerTypeID|ONGerListAcc] end
                end , [], NewPosList).

%% 阵容对比
cs_ger_view_other(#cs_ger_view_other{tarRoleID=TarRoleID, serverID=TarServerID}) ->
    RoleID = role_data:get_roleID(),
    case TarServerID =/= 0 of
        false ->
            case erlang:whereis(role_lib:regName(TarRoleID)) of
                Pid when erlang:is_pid(Pid) ->
                    role_lib:send_server(TarRoleID, {do_ger_view, RoleID, 0});
                ?undefined ->
                    do_offline_ger_view(0, 0, TarRoleID)
            end;
        true ->
            erlang:send(family_fight_server, {cs_fighter_ger_view_other,RoleID,data_setting:get(server_id),TarRoleID,TarServerID})
    end.

cs_ger_view_other_dtl(#cs_ger_view_other_dtl{tarRoleID=TarRoleID, serverID=TarServerID}) ->
    RoleID = role_data:get_roleID(),
    case TarServerID =/= 0 of
        false ->
            case erlang:whereis(role_lib:regName(TarRoleID)) of
                Pid when erlang:is_pid(Pid) ->
                    role_lib:send_server(TarRoleID, {do_ger_view_dtl, RoleID, 0});
                ?undefined ->
                    do_offline_ger_view_dtl(0, 0, TarRoleID)
            end;
        true ->
            erlang:send(family_fight_server, {cs_fighter_ger_view_other_dtl,RoleID,data_setting:get(server_id),TarRoleID,TarServerID})
    end.

%% (v4.0.0更改)吞噬果实,获得经验
cs_ger_eat(#cs_ger_eat{gerID=GerID,itemTypeID=ItemTypeID,itemNum=ItemNum}) ->
    case check_eat(GerID,ItemTypeID,ItemNum) of
        {true,MaxFeedNum,AddExp,SrcGer,PosList2,LPosList2,GerBag2,Type,BagOther,ItemDelAcc,ItemUpdateAcc,ItemUpdateLogList} ->
            do_eat(GerID,SrcGer,MaxFeedNum,AddExp,PosList2,LPosList2,GerBag2,Type,BagOther,ItemDelAcc,ItemUpdateAcc,ItemUpdateLogList);
        {false, Reason} ->
            ?sendself(#sc_ger_eat{gerID=GerID,result=Reason})
    end.

% v4.0取消品阶退化(4.1还原)
cs_ger_down_rank(#cs_ger_down_rank{srcGerID=SrcGerID}) ->
    case check_down_rank(SrcGerID) of
        {true, SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,Type,NeedGold} ->
            do_down_ger(SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,Type,NeedGold);
        {false, Reason} ->
            ?sendself(#sc_ger_down_rank{result=Reason,add_item_list=[],gerID=SrcGerID})
    end.
    


cs_ger_change_tag(#cs_ger_change_tag{tagID=TagID}) ->
    case check_can_change_tag(TagID) of
        {true,TagD,TagData,IconData} ->
            do_change_tag(TagID,TagD,TagData,IconData);
        {false,Reason} ->
            ?sendself(#sc_ger_change_tag{result=Reason,gerPosInfoList = [],lieuPosInfoList=[]})
    end.
cs_ger_saved_tag(#cs_ger_saved_tag{tagID=TagID}) ->
    case check_can_change_tag(TagID) of
        {true,TagD,_,_} ->
            ?sendself(#sc_ger_saved_tag{saveList=[TagD]});
        {false,_Reason} ->
            ?sendself(#sc_ger_saved_tag{saveList=[]})
    end.

cs_ger_set_tag_data(#cs_ger_set_tag_data{tagID=TagID,gerList=GL,lieuList=LL}) ->
    case GL of [] -> ?sendself(#sc_ger_set_tag_data{result=2});
        _ -> 
            case check_tagID_open(TagID,-1) of
                {true,_} ->
                    {NowTagID,TagData,IconData} = role_data:get_tag_data(),
                    NowTagD = #p_ger_tag_dtl{tag=TagID,gerList=GL,lieuList=LL},
                    NewTagData = lists:keystore(TagID, #p_ger_tag_dtl.tag, TagData,NowTagD ),
                    role_data:set_tag_data({NowTagID,NewTagData,IconData}),
                    ?sendself(#sc_ger_set_tag_data{result=1});
                _->  ?sendself(#sc_ger_set_tag_data{result=2})
            end
    end.

cs_ger_buy_tag(#cs_ger_buy_tag{tagID=TagID}) ->
    case check_can_buy_tag(TagID) of
        {true,NowTagID,Data,NeedGold,RoleInfo,IconData} ->
            do_buy_tag(TagID,NowTagID,Data,NeedGold,RoleInfo,IconData);
        {false,Reason} ->
            ?sendself(#sc_ger_buy_tag{result=Reason})
    end.

cs_ger_set_icon(#cs_ger_set_icon{iconList=IconList}) ->
    {A,B,_} = role_data:get_tag_data(),
    role_data:set_tag_data({A,B,IconList}),
    ?sendself(#sc_ger_set_icon{}).

check_can_buy_tag(TagID) ->
    {NowTagID,TagData,IconData} = role_data:get_tag_data(),
    case check_tagID_open(TagID,NowTagID) of
        false -> {false,2};
        {true,1} -> {false,3};
        {true,0} ->
            case lists:keyfind(TagID, #p_ger_tag_dtl.tag, TagData) of
                #p_ger_tag_dtl{} -> {false,3};
                _-> CostList = data_common:get(openTagCost),
                    {_,NeedGold} = lists:keyfind(TagID, 1, CostList),
                    Role = role_data:get_roleInfo(),
                    case role_lib:check_money(Role, gold, NeedGold) of
                        true -> {true,NowTagID,TagData,NeedGold,Role,IconData};
                        false -> {false,4}
                    end
            end
    end.

do_buy_tag(TagID,NowTagID,TagData,NeedGold,Role,IconData) ->
    TagD = (hd(TagData))#p_ger_tag_dtl{tag=TagID},
    TagData2 = lists:keysort(#p_ger_tag_dtl.tag, [TagD|TagData]),
    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_GER_BUY_TAG, TagID, ""),
    role_data:set_tag_data({NowTagID,TagData2,IconData}),
    ?sendself(#sc_ger_buy_tag{result=1}).
    
check_tagID_open(TagID,TagID) -> false;
check_tagID_open(TagID,_NowTagID) ->
    DefaultOpen =  data_common:get(default_open_tagIDList),
    case lists:member(TagID, DefaultOpen) of
        true -> {true,1};
        false -> 
            MaxTagID = data_common:get(max_tagID) ,
            if TagID > MaxTagID orelse TagID =< 0 -> false;
               true -> {true,0}
            end
    end.

check_can_change_tag(TagID) ->
    {NowTagID,TagData,IconData} = role_data:get_tag_data(),
    %if TagID == NowTagID orelse TagID == 0 orelse TagID > 10 -> {false,2};
    case check_tagID_open(TagID,NowTagID) of 
        false -> {false,2};
       {true,State} ->
           case lists:keyfind(TagID,#p_ger_tag_dtl.tag, TagData) of
               #p_ger_tag_dtl{}=D -> {true, D,TagData,IconData};
               false -> %OpenTagList  = data_common:get(tag_open_info),
                        %#role{level=Level} = role_data:get_roleInfo(),
                        %{_,TagNeedLevel} = lists:keyfind(TagID, 1, OpenTagList),
                        %if Level >= TagNeedLevel ->
                        if State == 1 ->
                               TagD = (hd(TagData))#p_ger_tag_dtl{tag=TagID},
                               TagData2 = lists:keysort(#p_ger_tag_dtl.tag, [TagD|TagData]),
                               {true, TagD,TagData2,IconData};
                           true -> {false,3}
                        end
           end
    end.
    
do_change_tag(TagID,TagD,TagData,IconData) ->
    {AllEquipB,AllGerB,AllItemBagB} = get_all_equip_and_ger(),
    {PosListN,LPosListN,EquipN,EquipBag,GerBag,ItemBag} = tag_stand_up(TagD,AllEquipB,AllGerB,AllItemBagB),
    [role_data:set_equip(GerID,GerTypeID,EquipList3)||{GerID,GerTypeID,EquipList3}<-EquipN],
    role_data:init_posList(PosListN),
    role_data:init_lieuList(LPosListN),
    role_data:set_gerBag(GerBag),
    role_data:set_bagEquip(EquipBag),
    role_data:set_tag_data({TagID,TagData,IconData}),
    role_data:set_bagItem(ItemBag),
    GerPosList = [begin
                      EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],
                      GerDimandInfo = GerHolyGrailInfo#holyGrail.diamondInfo,                 
                      #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList,diamondList=[role_diamond:to_p_diamond(E)||E<-GerDimandInfo]}
                  end ||#ger{gerBase=#gerBase{gerPos=Pos,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID}<-role_data:get_posList()],
    LieuPosList = [begin
                       EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],                   
                       #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList,diamondList=[role_diamond:to_p_diamond(E)||E<-GerHolyGrailInfo#holyGrail.diamondInfo]}
                   end ||#ger{gerBase=#gerBase{gerPos=Pos,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID}<-role_data:get_lieuposList()],
    ?sendself(#sc_ger_change_tag{result=1,gerPosInfoList=GerPosList,lieuPosInfoList=LieuPosList}).

tag_stand_up(TagD,AllEquipB,AllGerB,AllItemBagB) ->
    {PosList,Equiped,EquipBag,GerBag,ItemBag} = 
    lists:foldl(fun(#p_ger_save_dtl{gerUID=GerUID,pos=Pos,itemList=ItemList,diamondList=DiamondList},{A,B,C,D,Z}) ->
                        case lists:keytake(GerUID, #gerSimple.gerID, D) of
                            false -> {A,B,C,D,Z};
                            {value,GerSimple=#gerSimple{gerTypeID=GerTypeID,gerHolyGrailInfo=GerHolyGrailInfo},OGers} -> 
                                {GerEquipedEquip,OtherEquipBag} = 
                                    lists:foldl(fun(#p_ger_save_item{pos=IPos,itemUID=ItemUID},{E,F})->
                                                        case lists:keytake(ItemUID, #item.itemUID, F) of
                                                            false -> {E,F};
                                                            {value,It,OF} ->{[It#item{itemPos=IPos}|E],OF}
                                                        end
                                                end,{[],C},ItemList),
                                {GerDiamondList,OtherBagItem} = 
                                    lists:foldl(fun(#p_ger_save_item{pos=IPos,itemUID=ItemTypeID},{GDAcc,OBIAcc}) ->
                                                        case lists:keytake(ItemTypeID, #item.itemTypeID, OBIAcc) of
                                                            false -> {GDAcc,OBIAcc};
                                                            {value,Item=#item{itemNum=INum},OBIAcc2} -> 
                                                                {[#diamond_unit{diamondID=ItemTypeID,pos=IPos}|GDAcc],[Item#item{itemNum=INum-1}|OBIAcc2]}
                                                        end end, {[],Z}, DiamondList),
                                GerHolyGrailInfo2 = GerHolyGrailInfo#holyGrail{diamondInfo=GerDiamondList},
                                GerSimple2 = GerSimple#gerSimple{gerPos=Pos,gerHolyGrailInfo=GerHolyGrailInfo2},
                                {[GerSimple2|A],[{GerUID,GerTypeID,GerEquipedEquip}|B],OtherEquipBag,OGers,OtherBagItem}
                        end
                end,{[],[],AllEquipB,AllGerB,AllItemBagB},TagD#p_ger_tag_dtl.gerList),
    {LieuPosList,GerBag2} = 
        lists:foldl(fun(#p_ger_save_dtl{gerUID=LGerUID,pos=Pos},{Y,Z}) ->
                            case lists:keytake(LGerUID, #gerSimple.gerID, Z) of
                                false -> {Y,Z};
                                {value,LGerSimple,OGers} -> {[LGerSimple#gerSimple{gerPos=Pos}|Y],OGers}
                            end end,{[],GerBag},TagD#p_ger_tag_dtl.lieuList),
    {PosList,LieuPosList,Equiped,EquipBag,GerBag2,ItemBag}.
                                                

get_all_equip_and_ger() ->
    PosListB = role_data:get_posList(),
    {GerEquipList,GerDiamonds}
        = lists:foldl(fun(#ger{gerID=GerID,gerBase=#gerBase{gerHolyGrailInfo=GerHolyGrail}},{EAcc,DAcc}) ->
                              EquipList = role_data:get_equip(GerID),
                              Equipss = [EE#item{itemPos=0}||EE<-EquipList],
                              role_data:del_equip(GerID),
                              Diamonds = %[TypeID||#diamond_unit{diamondID=TypeID}<-GerHolyGrail#holyGrail.diamondInfo],
                                lists:foldl(fun(#diamond_unit{diamondID=T},Accd) ->
                                                    case lists:keytake(T, 1, Accd) of
                                                        false -> [{T,1}|Accd];
                                                        {value,{_,Nc},ODd} -> [{T,Nc+1}|ODd]
                                                    end end, DAcc, GerHolyGrail#holyGrail.diamondInfo),
                              {Equipss++EAcc,Diamonds}
                      end,{[],[]},PosListB),
    BagEquipB = role_data:get_bagEquip(),
    AllEquipB = GerEquipList++BagEquipB,
    ItemBag = role_data:get_bagItem(),
    ItemBag2 = lists:foldl(fun({DiamondID,Num},BagAcc)->
                                   {ItemBag1,_,_} = role_diamond:add_diamond_num_for_demount(DiamondID,Num,BagAcc),
                                   ItemBag1
                           end,ItemBag,GerDiamonds),
    GerBagB = role_data:get_gerBag(),
    LieuPosList = role_data:get_lieuposList(),
    GerFomation = lists:foldl(fun(#ger{gerBase=Base}=Ger,Acc) ->
                                      GerSimple=#gerSimple{gerExp=Base#gerBase.gerExp,gerID=Ger#ger.gerID
                                                          ,gerTypeID=Base#gerBase.gerTypeID,
                                                           gerQuality=Base#gerBase.gerQuality
                                                           ,gerLevel=Base#gerBase.gerLevel,gerPos=0
                                                           ,gerAwakeInfo=Base#gerBase.gerAwakeInfo
                                                           ,gerCrystalInfo=Base#gerBase.gerCrystalInfo
                                                           ,gerBody=Base#gerBase.gerBody
                                                           ,gerHolyGrailInfo=Base#gerBase.gerHolyGrailInfo
                                                          },
                                      [GerSimple|Acc]
                              end,[],PosListB++LieuPosList),
    AllGerB = GerFomation ++ GerBagB,
    {AllEquipB,AllGerB,ItemBag2}.
    


check_down_rank(SrcGerID) ->
    case is_mirror_ger(SrcGerID) of
        true ->
            {false, 5};
        false ->
            case role_data:get_ger(SrcGerID) of     
                {value, SrcGer, PosList2, LPosList2, GerBag2, Type} ->
                    if is_record(SrcGer, gerSimple) ->
                           #gerSimple{gerQuality=GerQuality,gerTypeID=GerTypeID} = SrcGer;
                       true ->
                           #ger{gerBase=#gerBase{gerQuality=GerQuality,gerTypeID=GerTypeID}} = SrcGer
                    end,
                    case util:is_exp_card(GerTypeID) of
                        false ->
                            case GerQuality > 0 of 
                                true ->
                                    Role = role_data:get_roleInfo(),
                                    NeedGold = data_common:get(down_ger_need_gold),
                                    case Role#role.gold + Role#role.goldBonus >= NeedGold of
                                        false ->
                                            {false,4}; 
                                        true ->
                                            NewQuality = GerQuality - 1,
                                            {true, SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,Type,NeedGold}
                                    end;
                                false ->
                                    {false, 3}
                            end;
                        true ->
                            {false, 2}
                    end;
                false ->
                    {false,1}
            end
    end.

do_down_ger(SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,Type,NeedGold) ->
    #role{roleID=RoleID} = Role,
    role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_GER_DOWN_RANK, 0, ""),
    if is_record(SrcGer, gerSimple) ->
           #gerSimple{gerQuality=OldQuality,gerLevel=SrcGerLevel,gerTypeID=GerTypeID,gerAwakeInfo=OldAwakeInfo} = SrcGer,
           NewAwakeInfo = role_awake:calculate_awakeinfo_down_rank(OldAwakeInfo,NewQuality,GerTypeID),
           SrcGer2 = SrcGer#gerSimple{gerQuality=NewQuality,gerAwakeInfo=NewAwakeInfo},
           SrcGer3 = SrcGer2,
           PosList3 = PosList2,
           LPosList3 = LPosList2,
           GerBag3 = [SrcGer3|GerBag2];
       true->
           #ger{gerBase=#gerBase{gerQuality=OldQuality,gerLevel=SrcGerLevel,gerTypeID=GerTypeID,gerAwakeInfo=OldAwakeInfo}=GerBase} = SrcGer,
           NewAwakeInfo = role_awake:calculate_awakeinfo_down_rank(OldAwakeInfo,NewQuality,GerTypeID),
           GerBase2 = GerBase#gerBase{gerQuality= NewQuality,gerAwakeInfo=NewAwakeInfo},
           SrcGer2 = SrcGer#ger{gerBase=GerBase2},
           if Type =:= ger ->
                  SrcGer4 = role_crystal:refresh_gercrystal(SrcGer2),
                  SrcGer3 = ger_attr:recacl(SrcGer4, PosList2),
                  PosList3 = [SrcGer3|PosList2],
                  LPosList3 = LPosList2,
                  GerBag3 = GerBag2;
              true ->
                  SrcGer3 = ger_attr:recacl_lieu(SrcGer2, LPosList2),
                  PosList3 = PosList2,
                  LPosList3 = [SrcGer3|LPosList2],
                  GerBag3 = GerBag2
           end
    end,    
    behavior_ger_downrank:log(RoleID, SrcGerID, GerTypeID, SrcGerLevel, OldQuality, NewQuality, erlang:localtime()),
    %%操作类型5作为精灵品阶下降操作符
    role_awake:add_ger_awake_log(RoleID,SrcGer,0,OldAwakeInfo,NewAwakeInfo,5),
    role_data:set_gerBag(GerBag3),
    ger_lib:notify_update(SrcGer3),
    %% 提醒客户端更新武将
    role_data:set_lieuposList(LPosList3),
    PosList4 = ger_attr:refresh_fightPower(PosList3),
    role_data:set_posList(PosList4),
    EggID = get_ger_egg_typeid(GerTypeID),
    %%20阶以上的返还的精灵蛋数目不同,所以此处要处理
    Num = get_down_rank_return_egg_num(GerTypeID,NewQuality),
    role_reward:handle_item_f(role_data:get_roleInfo(),[#new_item{itemTypeID = EggID,itemNum =Num,itemRank=0,itemLevel=1}],?MONEY_ADD_TYPE_GER_DOWN_RANK,0,""),
    ?sendself(#sc_ger_down_rank{result=0, add_item_list=role_reward:transform2p_reward_view([{6,EggID,Num}],[]),gerID=SrcGerID}),
    #data_ger{gerStar=GerStarLevel} = data_ger:get(GerTypeID),
    role_lvlSgAttr:on_ger_rank_up(GerStarLevel, OldQuality, NewQuality).

%% 升品，
cs_ger_up_rank(#cs_ger_up_rank{foodGerID=FoodGerID,srcGerID=SrcGerID}) ->
    case check_up_rank(FoodGerID, SrcGerID) of
        {true, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3,LPosList3, GerBag3,Type, FoodType,SrcGerLevel,SrcGerExp,FoodGerExp,UpType} ->
            do_up_rank(SrcGerID, FoodGerID, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3,LPosList3,  GerBag3,Type, FoodType,SrcGerLevel,SrcGerExp,FoodGerExp,UpType);
        {true, SrcGerID, Role, SrcGer, NewQuality,PosList3,LPosList3,GerBag3,Type, BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu,UpType}->
            do_up_rank2(SrcGerID, Role, SrcGer, NewQuality, PosList3,LPosList3, GerBag3,Type, BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu,UpType);
        {false, Reason} ->
            ?sendself(#sc_ger_up_rank{foodGerID=FoodGerID,srcGerID=SrcGerID,result=Reason})
    end.
    
%%v4.0精灵升品(v4.1.0版本还原成消耗精灵，故此处的代码注释掉)
% cs_ger_up_rank(#cs_ger_up_rank{gerID=GerID,upRankNum=UpRankNum}) ->
%     IsOpen = data_ger_up_rank:get(is_open),
%     case IsOpen of
%         true->
%             case role_data:get_ger(GerID) of
%                 {value,Ger,PosList2,LPosList2,GerBag2,Type}->
%                     Role  = role_data:get_roleInfo(),
%                     case Type of
%                         bag->
%                             #gerSimple{gerTypeID=GerTypeID,gerQuality=GerQuality} = Ger,
%                             case do_ger_up_rank(GerID,GerTypeID,GerQuality,UpRankNum,GerBag2) of
%                                 {false,R}->
%                                     ?sendself(#sc_ger_up_rank{result=R});
%                                 {NewGerQuality,NewGerBag,NewEquipBag,NewItemBag,DeleteCurrencyList}->
%                                     NewGer = Ger#gerSimple{gerQuality=NewGerQuality},
%                                     role_item:update_role_info2(NewItemBag,[NewGer|NewGerBag],NewEquipBag,DeleteCurrencyList,?MONEY_DEC_TYPE_GER_UP_RANK,0,integer_to_list(GerID)++"|"++ integer_to_list(GerQuality)++"|"++integer_to_list(NewGerQuality)),
%                                     ger_lib:notify_update(NewGer),
%                                     #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
%                                     broadcast_ger_update(NewGer,Role,GerStar),
%                                     role_lvlSgAttr:on_ger_rank_up(GerStar,GerQuality,NewGerQuality),
%                                     role_payGuide:update_uprank(GerTypeID,NewGerQuality),
%                                     role_gather:add_blink_ger_manual([{GerTypeID,NewGerQuality}]),
%                                     role_gather:hook_add_ger_list([{GerTypeID,NewGerQuality}]),
%                                     ?sendself(#sc_ger_up_rank{result=1,gerID=GerID,upRankNum=NewGerQuality-GerQuality})
%                             end;
%                         lieu->
%                             #ger{gerBase=GerBase} = Ger,
%                             #gerBase{gerTypeID=GerTypeID,gerQuality=GerQuality} = GerBase,
%                             case  do_ger_up_rank(GerID,GerTypeID,GerQuality,UpRankNum,GerBag2) of
%                                 {false,R}->
%                                     ?sendself(#sc_ger_up_rank{result=R});
%                                 {NewGerQuality,NewGerBag,NewEquipBag,NewItemBag,DeleteCurrencyList}->
%                                     #ger{gerBase=GerBase} = Ger,
%                                     NewGer = Ger#ger{gerBase=GerBase#gerBase{gerQuality=NewGerQuality}},
%                                     role_data:set_lieuposList([NewGer|LPosList2]),
%                                     role_item:update_role_info2(NewItemBag,NewGerBag,NewEquipBag,DeleteCurrencyList,?MONEY_DEC_TYPE_GER_UP_RANK,0,integer_to_list(GerID)++"|"++ integer_to_list(GerQuality)++"|"++integer_to_list(NewGerQuality)),
%                                     ger_lib:notify_update(NewGer),
%                                     #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
%                                     broadcast_ger_update(NewGer,Role,GerStar),
%                                     role_lvlSgAttr:on_ger_rank_up(GerStar,GerQuality,NewGerQuality),
%                                     role_payGuide:update_uprank(GerTypeID,NewGerQuality),
%                                     role_gather:add_blink_ger_manual([{GerTypeID,NewGerQuality}]),
%                                     role_gather:hook_add_ger_list([{GerTypeID,NewGerQuality}]),
%                                     ?sendself(#sc_ger_up_rank{result=1,gerID=GerID,upRankNum=NewGerQuality-GerQuality})
%                             end;                    
%                         ger->
%                             #ger{gerBase=GerBase} = Ger,
%                             #gerBase{gerTypeID=GerTypeID,gerQuality=GerQuality} = GerBase,
%                             case do_ger_up_rank(GerID,GerTypeID,GerQuality,UpRankNum,GerBag2) of
%                                 {false,R}->
%                                     ?sendself(#sc_ger_up_rank{result=R});
%                                 {NewGerQuality,NewGerBag,NewEquipBag,NewItemBag,DeleteCurrencyList}->
%                                     #ger{gerBase=GerBase} = Ger,
%                                     NewGer = Ger#ger{gerBase=GerBase#gerBase{gerQuality=NewGerQuality}},
%                                     NewGer1 = role_crystal:refresh_gercrystal(NewGer),
%                                     NewGer2 = ger_attr:recacl(NewGer1,PosList2),        
%                                     PosList4 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
%                                     role_data:set_posList(PosList4),
%                                     role_item:update_role_info2(NewItemBag,NewGerBag,NewEquipBag,DeleteCurrencyList,?MONEY_DEC_TYPE_GER_UP_RANK,0,integer_to_list(GerID)++"|"++integer_to_list(GerQuality)++"|"++integer_to_list(NewGerQuality)),
%                                     role_payGuide:trigger_task_change(?FORMATION_GER_STAR_N_QUALITY_N_ISMEGA_TN,{GerID}),
%                                     #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
%                                     broadcast_ger_update(NewGer2,Role,GerStar),
%                                     role_lvlSgAttr:on_ger_rank_up(GerStar,GerQuality,NewGerQuality),
%                                     role_payGuide:update_uprank(GerTypeID,NewGerQuality),  
%                                     role_gather:add_blink_ger_manual([{GerTypeID,NewGerQuality}]),
%                                     role_gather:hook_add_ger_list([{GerTypeID,NewGerQuality}]),
%                                     ?sendself(#sc_ger_up_rank{result=1,gerID=GerID,upRankNum=NewGerQuality-GerQuality})
%                             end
%                     end;                 
%                 _ ->
%                     ?sendself(#sc_ger_up_rank{result=2})
%             end;
%         _ ->
%             ?sendself(#sc_ger_up_rank{result=0})
%     end.

%%精灵变化的跑马灯推送
broadcast_ger_update(Ger,Role,GerStar)->
    case GerStar >= 5 of
        true->
            broadcast_server:bc(#sc_message_ger_upLevel{roleName=Role#role.roleName,gerInfo=ger_lib:ger2p_ger_view(Ger)});
        _->
            ignore
    end.

% do_ger_up_rank(GerID,GerTypeID,GerQuality,UpQualityNum,GerBag)->
%     case check_up_rank(GerTypeID,GerQuality,UpQualityNum) of
%         {false,R}->
%             {false,R};
%         {true,MaxUpQualityNum}->
%             ItemBag = role_data:get_bagItem(),
%             EquipBag = role_data:get_bagEquip(),
%             RoleInfo = role_data:get_roleInfo(),
%             case calculate_ger_up_rank_time(GerTypeID,GerQuality,MaxUpQualityNum,ItemBag,EquipBag,GerBag,[],true) of
%                 {GerQuality,_,_,_,_}->
%                     {false,5};
%                 {NewGerQuality,ItemBag1,EquipBag1,GerBag1,DeleteCurrencyList}->
%                     {NewGerQuality,GerBag1,EquipBag1,ItemBag1,DeleteCurrencyList};
%                 _ ->
%                     {false,5}
%             end
%     end.

% check_up_rank(GerTypeID,GerQuality,UpQualityNum)->
%     case util:is_exp_card(GerTypeID) of
%         true->
%             {false,3};
%         false->
%             case check_ger_quality_limit(GerTypeID,GerQuality,UpQualityNum) of
%                 {false,_}->
%                     {false,4};
%                 {true,MaxUpQualityNum}=R->
%                     R
%             end
%     end.

get_transform(_,_Condition,[],_) -> {false,[],void};
get_transform(Need,Condition,[Need|_T],Count) ->
    Transform = erlang:list_to_atom("transform"++erlang:integer_to_list(Count)),
    {true,Condition,Transform};
get_transform(Need,Condition,[_|T],Count) -> get_transform(Need,Condition,T,Count+1).
check_up_rank(FoodGerID, SrcGerID) ->
    case role_data:get_ger(SrcGerID) of     
        {value, SrcGer, PosList2, LPosList2, GerBag2, Type} ->
            if is_record(SrcGer, gerSimple) ->
                   #gerSimple{gerQuality=GerQuality,gerTypeID=GerTypeID,gerLevel=SrcGerLevel,gerExp=SrcGerExp} = SrcGer;
               true ->
                   #ger{gerBase=#gerBase{gerQuality=GerQuality,gerTypeID=GerTypeID,gerLevel=SrcGerLevel,gerExp=SrcGerExp}} = SrcGer
            end,
            case util:is_exp_card(GerTypeID) of
                false ->
                    #data_ger{gerStar=GerStar,breakthroughIDList=BreakThroughIDList} = data_ger:get(GerTypeID),
                    QualityLimitList = get_breakthrough_Limit(BreakThroughIDList),
                    Need = lists:keyfind(GerQuality, 4, QualityLimitList) ,
                    {CanUpRank, NeedList, UpType} = 
                        case Need of
                            false ->
                                {true, [], evolution};
                            #data_ger_breakthrough{condition=Condition}->
                                if Condition =:= [] ->
                                       {false, [], void};
                                   true->
                                       get_transform(Need,Condition,QualityLimitList,1)
                                end
                        end,
                    case GerQuality < ?MAX_GER_RANK andalso CanUpRank of 
                        true ->
                            if 
                                NeedList =:= [] ->
                                    case take_ger(FoodGerID, PosList2, LPosList2, GerBag2) of
                                        {value, FoodGer, PosList3, LPosList3, GerBag3, FoodType} ->
                                            if is_record(FoodGer, gerSimple) ->
                                                   #gerSimple{gerQuality=FoodGerQuality,gerTypeID=FoodGerTypeID,gerExp=FoodGerExp} = FoodGer;
                                               true ->
                                                   #ger{gerBase=#gerBase{gerQuality=FoodGerQuality,gerTypeID=FoodGerTypeID,gerExp=FoodGerExp}} = FoodGer
                                            end,
                                            case util:is_exp_card(FoodGerTypeID) of
                                                false ->
                                                    case FoodType of
                                                        bag ->
                                                            #data_ger{gerStar=SrcGerStar} = data_ger:get(GerTypeID),
                                                            #data_ger{gerStar=FoodGerStar} = data_ger:get(FoodGerTypeID),
                                                            case SrcGerStar =:= FoodGerStar of
                                                                false ->
                                                                    {false,5};
                                                                true ->
                                                                    QualityLimitList2 = [E#data_ger_breakthrough.rank_condition||E<-QualityLimitList],
                                                                    case catch check_quality(GerQuality, FoodGerQuality, QualityLimitList2) of
                                                                        {false, Reason}->
                                                                            {false, Reason};
                                                                        {true, NewQuality} ->
                                                                            Role = role_data:get_roleInfo(),
                                                                            NeedCoinT = cacl_up_rank_coin(NewQuality, GerQuality, FoodGerQuality, GerStar,GerTypeID),
                                                                            NeedCoin = erlang:max(0,NeedCoinT),
                                                                            case Role#role.coin >= NeedCoin of
                                                                                true ->
                                                                                    case homestead_server:has_homestead_ger(Role#role.roleID, [FoodGerID]) of
                                                                                        true->
                                                                                            {false,12};
                                                                                        false->
                                                                                            {true, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3, LPosList3, GerBag3, Type, FoodType, SrcGerLevel,SrcGerExp,FoodGerExp,UpType}
                                                                                    end;
                                                                                false ->
                                                                                    {false, 6}
                                                                            end
                                                                    end
                                                                end;
                                                        _ ->
                                                            {false,4}
                                                    end;
                                                true ->
                                                    {false, 11}
                                            end;
                                        _ ->
                                            case is_mirror_ger(FoodGerID) of
                                                true ->
                                                    {false, 14};
                                                false ->
                                                    {false, 4}
                                            end
                                    end;
                                true ->
                                    if
                                        FoodGerID =:= 0 ->
                                            Role = role_data:get_roleInfo(),
                                            BagItem = role_data:get_bagItem(),
                                            case check_need_list(Role, NeedList, BagItem) of
                                                false ->
                                                    {false,8}; 
                                                {BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu} ->
                                                    NewQuality = erlang:min(GerQuality + 1, ?MAX_GER_RANK),
                                                    {true, SrcGerID, Role, SrcGer, NewQuality,PosList2, LPosList2, GerBag2,Type, BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu,UpType}
                                            end;
                                        true ->
                                            {false, 13}
                                    end
                            end;
                        false ->
                            {false, 2}
                    end;
                true ->
                    {false, 10}
            end;
        false ->
            case is_mirror_ger(FoodGerID) of
                true ->
                    {false,14};
                false ->
                    {false,3}
            end
    end.
%%MAX_UP_RANK升级方式不能跨越转生限制，只能通过1级升级才能完成转生
check_ger_quality_limit(GerTypeID,GerQuality,?UP_RANK_ONE)->
    #data_ger{gerStar=Star} = data_ger:get(GerTypeID),
    RemainMaxQuality = data_ger_up_rank:get({data_ger_max_quality,Star})-GerQuality,
    {RemainMaxQuality>0,?UP_RANK_ONE};
check_ger_quality_limit(GerTypeID,GerQuality,?MAX_UP_RANK)->
    #data_ger{gerStar=Star,sameType=SameType} = data_ger:get(GerTypeID),
    IsSameType = case SameType of GerTypeID -> true; _ -> false end,
    MaxQuality =   data_ger_up_rank:get({data_ger_max_quality,Star}),
    case GerQuality < MaxQuality of
        false->
            {false,0};
        true->
            CMaxQuality = find_max_quality_before_transmigration(Star,GerQuality,IsSameType),
            RemainMaxQuality = CMaxQuality-GerQuality,
            {RemainMaxQuality>0,RemainMaxQuality}
    end;
check_ger_quality_limit(_GerTypeID,_GerQuality,UpRankType)->
    ?ERR("undefined uprank type:~w~n",[UpRankType]),
    {false,0}.
%%递归的确定对应星级当前品阶的状况下能够升的最大等级
find_max_quality_before_transmigration(GerStar,InitQuality,_IsSameType)->
    KeyQualityL = data_ger_up_rank:get(key_quality_list),
    MaxQuality = data_ger_up_rank:get({data_ger_max_quality,GerStar}),
    case InitQuality >=MaxQuality of
        true->
            0;
        false->
            find_max_quality_before_transmigration2(KeyQualityL,InitQuality)
    end.
find_max_quality_before_transmigration2(KeyQualityL,Quality)->
    lists:min([K||K<-KeyQualityL,K>Quality])-1.

%% FIX （v4.1.0版本精灵进阶还原成吞噬精灵，故此处的代码需要注释掉）
%%递归的判断玩家是否满足升级精灵品阶的消耗
calculate_ger_up_rank_time(_GerTypeID,InitQuality,0,ItemBag,EquipBag,GerBag,DeleteCurrencyList,_IsEnough)->
  {InitQuality,ItemBag,EquipBag,GerBag,DeleteCurrencyList};
calculate_ger_up_rank_time(_GerTypeID,InitQuality,_Times,ItemBag,EquipBag,GerBag,DeleteCurrencyList,false)->
  {InitQuality,ItemBag,EquipBag,GerBag,DeleteCurrencyList};
calculate_ger_up_rank_time(GerTypeID,InitQuality,MaxUpTimes,ItemBag,EquipBag,GerBag,DeleteCurrencyList,true)->
  #data_ger{gerStar=Star,sameType=SameType} = data_ger:get(GerTypeID),
  %%此处将上次累计消耗的货币加入到item中，delete_sell_reward将会筛选对应的道具，形成新的货币删除列表，从而判断货币是否足够
  Type = case role_ger:is_specail_ger(GerTypeID) of
    true->
        2;
    false->
        case role_awake:check_ger_mega(GerTypeID) of
            true->
                3;
            false->
                1;
            ?undefined->
                1
        end
    end,
    {Cost=#sell_reward{item=OldItemList},_IsTransmigation} = 
        case SameType of GerTypeID -> data_ger_up_rank:get({Star,InitQuality+1,Type});
            _-> data_ger_up_rank_light:get({Star,InitQuality+1,Type})
        end,
    case role_item:delete_sell_reward(Cost#sell_reward{item=OldItemList++DeleteCurrencyList},[],[],GerBag,ItemBag,EquipBag) of
        {true,ItemBag1,GerBag1,EquipBag1,_LastDeleteList,_DeleteItemList,_DeleteEquipList,DeleteCurrencyList1}->
            calculate_ger_up_rank_time(GerTypeID,InitQuality+1,MaxUpTimes-1,ItemBag1,EquipBag1,GerBag1,DeleteCurrencyList1,true);
        false->
            calculate_ger_up_rank_time(GerTypeID,InitQuality,MaxUpTimes,ItemBag,EquipBag,GerBag,DeleteCurrencyList,false)
    end.



% 只允许上阵ger修改身份 v260
cs_ger_set_body(#cs_ger_set_body{gerID=GerID,gerBody=GerBody})->
    case role_data:get_ger(GerID) of
        {value, Ger, PosList2, _LPosList, _GerBag, ger} ->
          case check_set_body(Ger) of
            true->
              #ger{gerBase=GerBase}= Ger ,
              GerBase2 = GerBase#gerBase{gerBody=GerBody},
              SrcGer2 = Ger#ger{gerBase=GerBase2},
              PosList3 = [SrcGer2|PosList2],
              role_data:set_posList(PosList3),
              ?sendself(#sc_ger_set_body{result=1});
            false->
              ?sendself(#sc_ger_set_body{result=2})
          end;
        _ ->
            ?sendself(#sc_ger_set_body{result=2})
    end.

%%防止前端刷协议，此处对上阵的精灵的星等进行判断 v280
check_set_body(Ger) when is_record(Ger,ger)->
  #ger{gerBase=GerBase} = Ger,
  #data_ger{gerStar=Star} = data_ger:get(GerBase#gerBase.gerTypeID),
  lists:member(Star,data_ger_up_rank:get(data_change_body_list));
check_set_body(_Ger)->
  false.
  
do_up_rank(SrcGerID, FoodGerID, Role, SrcGer, FoodGer, NewQuality, NeedCoin, PosList3,LPosList3, GerBag3,Type,FoodType,SrcGerLevel,SrcGerExp,FoodExp,UpType) ->
	%?ERR("ger in up:~w,\n~w\n~w\n~w",[SrcGer,FoodGer, GerBag3,Type]),
	#role{roleID=RoleID} = Role,
	role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	if is_record(SrcGer, gerSimple) -> %% Type = bag
		   #gerSimple{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID} = SrcGer,
           {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,GerTypeID),
		   SrcGer2 = SrcGer#gerSimple{gerLevel=Level2,gerExp=Exp2,gerQuality=NewQuality},
		   if is_record(FoodGer, gerSimple) -> %% FoodType = bag
				  SrcGer3 = SrcGer2,
				  GerBag4 = [SrcGer3|GerBag3],
				  LPosList4 = LPosList3,
				  PosList4 = PosList3;
			  true ->
				  role_data:replace_equip(SrcGerID,GerTypeID,FoodGerID),
				  GerBaseT0 = FoodGer#ger.gerBase,
				  GerBaseT = GerBaseT0#gerBase{gerQuality=NewQuality,gerLevel=Level2,gerExp=Exp2},
				  SrcGerT = FoodGer#ger{gerID=SrcGerID, gerBase=GerBaseT},
				  if FoodType =:= ger -> %% FoodType = Ger
						 SrcGer3 = ger_attr:recacl(SrcGerT, PosList3),
						 PosList4 = [SrcGer3|PosList3],
						 LPosList4 = LPosList3,
						 GerBag4 = GerBag3;
					 true -> %% FoodType = lieu
						SrcGer3 = ger_attr:recacl_lieu(SrcGerT, LPosList3),
						GerBag4 = GerBag3,
						PosList4 = PosList3,
						LPosList4 = [SrcGer3|LPosList3]
				  end
		   end;
	   true ->
		   #ger{gerBase=#gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID}} = SrcGer,
           {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,GerTypeID),
		   %% 处理被吞噬的武将的装备
		   if is_record(FoodGer, gerSimple) -> %% FoodType = bag
				  ignore;
			  true ->				  
				  FoodEquip = role_data:get_equip(FoodGerID),
				  if FoodEquip == [] ->
						 ignore;
					 true ->		
						 BagEquip = role_data:get_bagEquip(),
						 BagEquip2 = FoodEquip ++ BagEquip,
						 role_data:set_bagEquip(BagEquip2)
				  end
		   end,
		   #ger{gerBase=GerBase} = SrcGer,
		   GerBase2 = GerBase#gerBase{gerExp=Exp2,gerLevel=Level2, gerQuality= NewQuality},
		   SrcGer2 = SrcGer#ger{gerBase=GerBase2},
		   if Type =:= ger -> %% Type = ger
		   		SrcGer4 = role_crystal:refresh_gercrystal(SrcGer2),
				  SrcGer3 = ger_attr:recacl(SrcGer4, PosList3),
				  ger_lib:notify_update(SrcGer3),
				  PosList4 = [SrcGer3|PosList3],
				  GerBag4 = GerBag3,
				  LPosList4 = LPosList3;
			  true -> %% Type = lieu
				  SrcGer3 = ger_attr:recacl_lieu(SrcGer2, LPosList3),
				  ger_lib:notify_update(SrcGer3),
				  LPosList4 = [SrcGer3|LPosList3],
				  PosList4 = PosList3,
				  GerBag4 = GerBag3
		   end
	end,
	%% 写武将消耗日志和升品日志
	{Date, _} = Time = erlang:localtime(),
	if is_record(FoodGer, gerSimple) ->
		   LogGerList = [[FoodGer#gerSimple.gerID,FoodGer#gerSimple.gerTypeID,FoodGer#gerSimple.gerLevel,FoodGer#gerSimple.gerQuality]];
	   true ->
		   #ger{gerBase=FoodGerBase} = FoodGer,
		   LogGerList = [[FoodGer#ger.gerID,FoodGerBase#gerBase.gerTypeID,FoodGerBase#gerBase.gerLevel,FoodGerBase#gerBase.gerQuality]]
	end,
    erlang:send(self(), {ger_del_cancel_cirrus, FoodGer#gerSimple.gerID}),
	behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, integer_to_list(SrcGerID)),
	behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, SrcGerLevel, SrcGerExp, SrcGerRank, Level2, Exp2, NewQuality, FoodGerID, Date, Time),
	
	
	role_data:set_gerBag(GerBag4),
	ger_lib:notify_update(SrcGer3),
	role_data:set_lieuposList(LPosList4),
	%% 刷新主将
	PosList5 = ger_attr:refresh_fightPower(PosList4),
	role_data:set_posList(PosList5),
	#role{roleName=RoleName} = Role,
	?CATCH(role_task_trigger:handle({dispach_task,ger_up_quality,SrcGerID,GerTypeID,NewQuality,UpType})),
	?sendself(#sc_ger_up_rank{foodGerID=FoodGerID, result=1,srcGerID=SrcGerID}),
    case data_ger:get(GerTypeID) of
        #data_ger{gerStar=GerStarLevel} ->
            role_lvlSgAttr:on_ger_lvl_up(GerStarLevel, SrcGerLevel, Level2),
            role_lvlSgAttr:on_ger_rank_up(GerStarLevel, SrcGerRank, NewQuality),
            case GerStarLevel >= 5 of
                true ->
                    broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(SrcGer3)});
                _ ->
                    next
            end;
        _ ->
            next
    end,
	role_homestead:hook_ger_delete(SrcGerID,NewQuality,Level2,[FoodGerID]),
		role_payGuide:update_uprank(GerTypeID,NewQuality).

do_up_rank2(SrcGerID, Role, SrcGer, NewQuality, PosList3, LPosList3, GerBag3, Type,BagItem2, DelItem, UpdateItem2, UpdateLogList,NeedCoin,NeedGold, NeedRepu,UpType)->
	#role{roleID=RoleID} = Role,
	role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	role_lib:deduct_reputation_f(Role, NeedRepu, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_GER_UP_RANK, 0, ""),
	if is_record(SrcGer, gerSimple) ->
		   #gerSimple{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID} = SrcGer,
		   SrcGer2 = SrcGer#gerSimple{gerQuality=NewQuality},
		   SrcGer3 = SrcGer2,
		   PosList4 = PosList3,
		   LPosList4 = LPosList3,
		   GerBag4 = [SrcGer2|GerBag3];
	   true->
		   #ger{gerBase=#gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerRank,gerTypeID=GerTypeID}=GerBase} = SrcGer,
		   GerBase2 = GerBase#gerBase{gerQuality= NewQuality},
		   SrcGer2 = SrcGer#ger{gerBase=GerBase2},
		   if Type =:= ger ->
          SrcGer4 = role_crystal:refresh_gercrystal(SrcGer2),
				  SrcGer3 = ger_attr:recacl(SrcGer4, PosList3),
				  ger_lib:notify_update(SrcGer3),
				  PosList4 = [SrcGer3|PosList3],
				  LPosList4 = LPosList3,
				  GerBag4 = GerBag3;
			  true ->
				  SrcGer3 = ger_attr:recacl_lieu(SrcGer2, LPosList3),
				  ger_lib:notify_update(SrcGer3),
				  PosList4 = PosList3,
				  LPosList4 = [SrcGer3|LPosList3],
				  GerBag4 = GerBag3
		   end
	end,
    role_gather:hook_add_ger_list([{GerTypeID,NewQuality}]),
	{Date, _} = Time = erlang:localtime(),
	if DelItem == [] andalso UpdateItem2 == [] ->
		   ignore;
	   true ->
		   %% 写道具日志
		   LogItemList = role_item:itemList2logItemList(DelItem, UpdateLogList),
		   behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_GER_UP_RANK, 1, ""),
		   role_data:set_bagItem(BagItem2),
		   DelItemIDList = [E||#item{itemUID=E}<-DelItem],
		   UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem2],
		   %% 提醒客户端更新物品
		   ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
		   ?sendself(#sc_item_update{updateList=UpdateList})
	end,
	behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, SrcGerLevel, SrcGerExp, SrcGerRank, SrcGerLevel, SrcGerExp, NewQuality, 0, Date, Time),
	role_data:set_gerBag(GerBag4),
	ger_lib:notify_update(SrcGer3),
	%% 提醒客户端更新武将
	role_data:set_lieuposList(LPosList4),
	PosList5 = ger_attr:refresh_fightPower(PosList4),
	role_data:set_posList(PosList5),
	#role{roleName=RoleName} = Role,
    ?CATCH(role_task_trigger:handle({dispach_task,ger_up_quality,SrcGerID,GerTypeID,NewQuality,UpType})),
	?sendself(#sc_ger_up_rank{foodGerID=0, result=1,srcGerID=SrcGerID}), 
		role_payGuide:update_uprank(GerTypeID,NewQuality),
    case data_ger:get(GerTypeID) of
        #data_ger{gerStar=GerStarLevel} ->
            role_lvlSgAttr:on_ger_rank_up(GerStarLevel, SrcGerRank, NewQuality),
            case GerStarLevel >= 5 of
                true ->
                    broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(SrcGer3)});
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

check_quality(GerQuality, FoodGerQuality, QualityLimitList)->
	case lists:member(GerQuality, QualityLimitList) of
        true ->
            erlang:throw({false, 9});
        false ->
            next
    end,
    MaxQuality = get_max_quality(GerQuality, lists:sort(QualityLimitList)),
    NextQuality = GerQuality + FoodGerQuality + 1,
    NewQuality = erlang:min(NextQuality, MaxQuality),
    {true, NewQuality}.

get_max_quality(_GerQuality, []) ->
    erlang:throw({false, 7});
get_max_quality(GerQuality, [Quality|QualityList]) ->
    case GerQuality < Quality of
        true ->
            Quality;
        false ->
            get_max_quality(GerQuality, QualityList)
    end.

check_need_list(Role, NeedList, BagItem)->
	if 	NeedList =:= [] ->
			false;
		true ->
			{BagItem2, Result, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu} =
				lists:foldl(fun({coin, Coin},{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc})->
									case Role#role.coin >= Coin of
										true ->
											{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc+Coin, NeedGoldAcc, NeedRepuAcc};
										false ->
											%?ERR("coin"),
											{BagItemAcc,false, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc}
									end;
							   ({gold, Gold},{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc})->
									case Role#role.gold + Role#role.goldBonus >= Gold of
										true ->
											{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc+Gold, NeedRepuAcc};
										false ->
											%?ERR("gold"),
											{BagItemAcc,false, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc,NeedRepuAcc}
									end;
							   ({reputation, Repu},{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc,NeedRepuAcc})->
									case Role#role.reputation >= Repu of
										true ->
											{BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc+Repu};
										false ->
											%?ERR("repu"),
											{BagItemAcc,false, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc}
									end;
							   ({item, ItemTypeID, Num}, {BagItemAcc, true, DelItemAcc, UpdataItemAcc, UpdateItemLogAcc,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc}) ->
									case item_lib:check_material2(BagItemAcc, ItemTypeID, Num) of
										{BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
											{BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogAcc++UpdateItemLogList,NeedCoinAcc, NeedGoldAcc, NeedRepuAcc};
										_ ->
											%?ERR("item"),
											{BagItemAcc, false, [],[],[],0,0,0}
									end;
							   (_, {_,false, _,_,_,_})->
									%?ERR("other"),
									{BagItem, false, [],[],[],0,0,0};
								(X,ACC) ->
									 ?ERR("unknow item in ger break throw:~w",[X]),
									ACC
							end,{BagItem, true, [], [], [],0,0,0},NeedList),
			case Result of
				true->
					{BagItem2, DelList, UpdateList, UpdateLogList, NeedCoin, NeedGold, NeedRepu};
				_ ->
					false
			end
	end.

get_breakthrough_Limit(BreakThroughIDList)->
	lists:map(fun(E)->data_ger_breakthrough:get(E) end, BreakThroughIDList).

%% 计算升品需要的银两
cacl_up_rank_coin(NewQuality, Quality1, Quality2, GerStar,GerTypeID) ->
	get_up_rank_coin(GerTypeID,NewQuality, GerStar) - get_up_rank_coin(GerTypeID,Quality1, GerStar) - get_up_rank_coin(GerTypeID,Quality2, GerStar).

get_up_rank_coin(0, _GerStar) ->
	0;
get_up_rank_coin(Quality, GerStar) ->
  get_up_rank_coin(0,Quality,GerStar).
%%由于精灵品阶增加到20阶以上，20阶以上进化消耗的配置有所不同，需要GerTypeID，故此处进行了扩展
get_up_rank_coin(_GerTypeID,0,_GerStar)->
  0;
get_up_rank_coin(GerTypeID,Quality,GerStar)->
    #data_ger{gerSecondEvolutionID=GerSecondEvolutionID,sameType=SameType} = data_ger:get(GerTypeID),
    case Quality > 20 of
        false->
            case SameType of
                GerTypeID -> data_ger_up_rank:get({GerStar, Quality});
                _-> data_ger_up_rank_light:get({GerStar,Quality})
            end;
        true->
            {CoinNum,_GerNum} = data_second_evolution:get({coinconsume,GerSecondEvolutionID,Quality-1}),
            CoinNum
    end.
			


-define(exp_ratio, 0.5).
cacl_exp(Ger) ->
	#gerSimple{gerExp=GerExp,gerTypeID=GerTypeID}=Ger,
	#data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
	BaseExp = 
	case GerStar of
		1 ->
			10;
		2 ->
			20;
		3 ->
			50;
		4 ->
			100;
		5 ->
			200;
		6 ->
			400;
		7 ->
			800;
		_ ->
			0
	end,
	BaseExp + trunc(?exp_ratio*GerExp).
			
do_eat(GerID,SrcGer,_MaxFeedNum,AddExp,PosList2,LPosList2,GerBag3,Type,BagOther,ItemDelAcc,ItemUpdateAcc,ItemUpdateLogList) ->
	#role{level=RoleLevel,roleID=RoleID} = role_data:get_roleInfo(),
	if is_record(SrcGer, gerSimple) ->
		   #gerSimple{gerExp=GerExp,gerQuality=GerRank,gerLevel=GerLevel,gerTypeID=GerTypeID} = SrcGer, 
		   #gerSimple{gerExp=NewExp,gerLevel=NewLevel} = SrcGer2 = ger_lib:add_exp_and_notify2(SrcGer, AddExp, RoleLevel),
		   GerBag4 = [SrcGer2|GerBag3],
		   role_data:set_gerBag(GerBag4),
		   role_data:set_lieuposList(LPosList2),
		   role_data:set_posList(PosList2);
	   true ->
		   #ger{gerBase=#gerBase{gerExp=GerExp,gerQuality=GerRank,gerLevel=GerLevel,gerTypeID=GerTypeID}} = SrcGer,
		   if Type =:= ger ->
				  {_, SrcGer2,_} = ger_lib:add_exp_and_notify(SrcGer, AddExp, RoleLevel, PosList2),
				  #ger{gerBase=#gerBase{gerExp=NewExp,gerLevel=NewLevel}} = SrcGer2,
				  PosList3 = [SrcGer2|PosList2],
				  role_data:set_gerBag(GerBag3),
				  role_data:set_lieuposList(LPosList2),
				  role_data:set_posList(PosList3),
                  role_payGuide:trigger_task_change(?FORMATION_GER_LEVEL_TN,{GerID,NewLevel}),
                  role_maintask:do_main_task(?FORMATION_GER_LEVEL_N_TN,{GerID,NewLevel});
			  true ->
				  {_, SrcGer2,_} = ger_lib:add_exp_and_notify_lieu(SrcGer, AddExp, RoleLevel, LPosList2),
				  #ger{gerBase=#gerBase{gerExp=NewExp,gerLevel=NewLevel}} = SrcGer2,
				  LPosList3 = [SrcGer2|LPosList2],
				  role_data:set_gerBag(GerBag3),
				  role_data:set_lieuposList(LPosList3),
				  role_data:set_posList(PosList2)
		   end
	end,
  %%更新道具背包
  LogItemList = role_item:itemList2logItemList(ItemDelAcc, ItemUpdateLogList),
  {Date, _} = Time = erlang:localtime(),
  behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_GER_EAT,0,integer_to_list(GerID)),
  item_lib:ra_item_update_num(ItemUpdateAcc),
  item_lib:ra_item_delete(ItemDelAcc),
  role_data:set_bagItem(BagOther),
  %%增加精灵消耗日志
	{Date, _} = Time = erlang:localtime(),
	behavior_ger_uplevel:log(RoleID, GerID, GerTypeID, GerLevel, GerExp, GerRank, NewLevel, NewExp, Date, Time),
	?CATCH(role_task_trigger:handle({dispach_task,ger_up_level,GerID,GerTypeID,NewLevel})),
	?sendself(#sc_ger_eat{result=1,gerID=GerID}).

do_set_body(SrcGer, PosList2, LPosList2, GerBag3, Type, BodyType) ->
    if is_record(SrcGer, gerSimple) ->
           SrcGer2 = SrcGer#gerSimple{gerBody=BodyType},
           GerBag4 = [SrcGer2|GerBag3],
           role_data:set_gerBag(GerBag4),
           role_data:set_lieuposList(LPosList2),
           role_data:set_posList(PosList2);
       true ->
           if Type =:= ger ->
                  #ger{gerBase=GerBase}= SrcGer ,
                  GerBase2 = GerBase#gerBase{gerBody=BodyType},
                  SrcGer2 = SrcGer#ger{gerBase=GerBase2},
                  PosList3 = [SrcGer2|PosList2],
                  role_data:set_gerBag(GerBag3),
                  role_data:set_lieuposList(LPosList2),
                  role_data:set_posList(PosList3);
              true ->
                  #ger{gerBase=GerBase}= SrcGer ,
                  GerBase2 = GerBase#gerBase{gerBody=BodyType},
                  SrcGer2 = SrcGer#ger{gerBase=GerBase2},
                  LPosList3 = [SrcGer2|LPosList2],
                  role_data:set_gerBag(GerBag3),
                  role_data:set_lieuposList(LPosList3),
                  role_data:set_posList(PosList2)
           end
    end,
    ?sendself(#sc_ger_set_body{result=1}).

gerList2logGerList(GerList) ->
	lists:map(fun(#ger{gerID=GerID,gerBase=GerBase}) ->
					  [GerID,GerBase#gerBase.gerTypeID,GerBase#gerBase.gerLevel,GerBase#gerBase.gerQuality];
				 (#gerSimple{gerID=GerID,gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality}) ->
					  [GerID, GerTypeID, GerLevel, GerQuality]
			  end, GerList).
	
max_ger_exp(GerTypeID) ->
	MaxLevel = data_common:get(max_ger_level),
    case ger_lib:is_blink_ger(GerTypeID) of
        false->
            data_ger_level:get(MaxLevel+1) -1;
        true->
            data_ger_level_light:get(MaxLevel+1)-1
    end.

check_eat(GerID, ItemTypeID,ItemNum) ->
  case role_ger:is_mirror_ger(GerID) of
    true ->
      {false,6};
    false ->
      case role_data:get_ger(GerID) of
        {value, SrcGer,PosList2, LPosList2, GerBag2, Type} ->
          %#role{level=Level} = role_data:get_roleInfo(),
          if 
            is_record(SrcGer,gerSimple) ->
              #gerSimple{gerTypeID=GerTypeID,gerLevel=_GerLevel,gerExp=GerExp}=SrcGer;
            true ->
              #ger{gerBase=SrcGerBase} = SrcGer,
              #gerBase{gerTypeID=GerTypeID,gerLevel=_GerLevel,gerExp=GerExp} = SrcGerBase
          end,
          case util:is_exp_card(GerTypeID) of
            false ->
              CurrentRoleLevelMaxGerExp = max_ger_exp(GerTypeID),
              MaxFeedExp = CurrentRoleLevelMaxGerExp - GerExp,
              case MaxFeedExp > 0 of
                false->
                  {false,4};
                true->
                  case caculate_fruit_real_feed_num(ItemTypeID,ItemNum,MaxFeedExp) of
                    {true,MaxFeedNum,RealFeedExp}->
                      case item_lib:check_material(ItemTypeID,MaxFeedNum) of
                        false->
                          {false,8};
                        {true, BagOther2, DelAcc, UpdateAcc, UpdateLogList}->
                          {true,MaxFeedNum,RealFeedExp,SrcGer,PosList2,LPosList2,GerBag2,Type,BagOther2,DelAcc,UpdateAcc,UpdateLogList}
                      end;
                    false->
                      {false,7}
                  end
              end;
            true->
              {false,5}
          end;
        _ ->
          {false,2}
      end
  end.

%%喂养数量是满足最大需求经验情况下的最小喂养数量
caculate_fruit_real_feed_num(ItemTypeID,ItemNum,MaxFeedExp)->
  case data_fruit:get({fruit_exp,ItemTypeID}) of
    ?undefined->
      false;
    FruitExp->
      Div = MaxFeedExp div FruitExp,
      Rem = MaxFeedExp rem FruitExp,
      MaxFeedNum = case Rem=:= 0 of true->Div;false->Div+1 end,
      case MaxFeedNum > ItemNum of
        true->
          {true,ItemNum,ItemNum*FruitExp};
        false->
          {true,MaxFeedNum,MaxFeedExp}
      end
  end.

do_offline_ger_view(SrcRoleID, SrcServerID, TarRoleID) ->
	case db_sql:get_roleInfo(TarRoleID) of
		#role{}=Role ->
			{FighterList,_,_,_TrSpecial} = role_data:get_otherRoleFighter(TarRoleID),
			Reply = ger_view_info(Role, FighterList);
		_ ->
			Reply = #sc_ger_view_other{tarRoleID=TarRoleID,roleName="",roleLevel=0,fightPower=0,gerList=[]}
	end,
    case SrcServerID of
        0 ->
            ?sendself(Reply);
        _ ->
            alien_server:send_msg_to_slave_sever_id(SrcServerID, {cross_ger_view_return, SrcRoleID, TarRoleID, Reply})
    end.
			

ger_view_info(RolePublic, FighterList) when is_record(RolePublic,rolePublic)->
	#rolePublic{roleID=TarRoleID,roleName=RoleName,fightPower=FightPower,level=Level}=RolePublic,
	GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
	#sc_ger_view_other{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower,gerList=GerViewList};
ger_view_info(Role, FighterList) ->
	#role{roleID=TarRoleID,roleName=RoleName,fightPower=FightPower,level=Level}=Role,
	GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
	#sc_ger_view_other{tarRoleID=TarRoleID,roleName=RoleName,roleLevel=Level,fightPower=FightPower,gerList=GerViewList}.

do_offline_ger_view_dtl(SrcRoleID, SrcServerID, TarRoleID) ->
	case db_sql:get_roleInfo(TarRoleID) of
		#role{}=Role ->
			%{FighterList, LieuInfoList} = db_sql:get_fighterList_and_lieuInfo(TarRoleID),
			{FighterList, AtkAdd,HpAdd,LieuInfoList,Special,_LieAdd} = db_sql:get_fighterList_and_lieu_add_info(TarRoleID),
			ItemList = role_data:get_otherRoleItemEquips(TarRoleID),
			SkinInfo = role_data:get_otherRoleSkinInfo(TarRoleID),
			Reply = ger_view_info_dtl(Role, FighterList, ItemList, AtkAdd, HpAdd,LieuInfoList,Special,SkinInfo);				
		_ ->
			% Reply = #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName="",isMale=false,roleLevel=0
   %                                        ,fightPower=0,gerList=[],equipList=[], atkAdd=0, hpAdd=0,lieuViewList=[]
   %                                        ,head=0,title=0,trID=0,specialID=0,skinInfo=#p_skin_info{equip=0},gerCrystalList=[]}
      Reply = #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName="",isMale=false,roleLevel=0
                                          ,fightPower=0,gerList=[],equipList=[], lieuViewList=[]
                                          ,head=0,title=0,trID=0,specialID=0,skinInfo=#p_skin_info{equip=0},gerCrystalList=[]}
	end,
    case SrcServerID of
        0 ->
            ?sendself(Reply);
        _ ->
            alien_server:send_msg_to_slave_sever_id(SrcServerID, {cross_ger_view_dtl_return, SrcRoleID, TarRoleID, Reply})
    end.

ger_view_info_dtl(Role, FighterListT, ItemList,AtkAdd,HpAdd,LieuInfoList,#trSpecial{trID=TrID,specialID=SpecialID},SkinInfo)->
	FighterList = ger_attr:refresh_other_fightPower(FighterListT,AtkAdd,HpAdd),
	#role{roleID = TarRoleID, roleName = RoleName, isMale=IsMale, fightPower=FightPower, level=Level,head = Head,title = Title}=Role,
	GerViewList = [ger_lib:ger2p_ger_view_dtl(E)||E<-FighterList],
    GerCrystalList =[ger_lib:ger2ger_crystalinfo_brief(E)||E<-FighterList],
	GerPosList = [ger_lib:ger2p_ger_pos(E)||E<-FighterList],
	EquipViewList = [item_lib:item2p_item_view_dtl(E)||E<-ItemList],
	PSkinInfo = role_skin:transforSkinInfo2PSkinInfo(SkinInfo),
	%?INFO("ger_view_info_dtl ~w ~w",[Head,Title]),
	% #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,isMale=IsMale,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
	% 					   ,equipList=EquipViewList,gerPosList=GerPosList,atkAdd=AtkAdd, hpAdd=HpAdd,lieuViewList=LieuInfoList
 %                            ,head=Head,title=Title,trID=TrID,specialID=SpecialID,skinInfo=PSkinInfo,gerCrystalList=GerCrystalList}.
  #sc_ger_view_other_dtl{tarRoleID=TarRoleID,roleName=RoleName,isMale=IsMale,roleLevel=Level,fightPower=FightPower, gerList=GerViewList
               ,equipList=EquipViewList,gerPosList=GerPosList,lieuViewList=alien_master_server:transformOldLiueView2NewLiueView(LieuInfoList)
                            ,head=Head,title=Title,trID=TrID,specialID=SpecialID,skinInfo=PSkinInfo,gerCrystalList=GerCrystalList}.
do_standup(GerPos, GerID, UpGer, PosList, GerBag2) ->
	GerTypeID = UpGer#gerSimple.gerTypeID,
	case util:fun_take(fun(E) ->GerPos == E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			GerTypeID2 = 0,
			%% 新的出站列表
			UpGer3 = role_crystal:refresh_gercrystal(UpGer),
			UpGer2 = ger_attr:recacl(UpGer3#gerSimple{gerPos=GerPos}, PosList),
			PosList2 = [UpGer2|PosList],
			%% 重算天命
			PosList4 = [UpGer2|[destiny_impact(E,GerTypeID, GerTypeID2, PosList2)||E<-PosList]],
			?CATCH(role_task_trigger:handle({dispach_task,role_up_ger_num,length(PosList4)})),
			GerBag3 = GerBag2;
		{value, #ger{gerID=GerID2,gerBase=#gerBase{gerTypeID=GerTypeID2}}=DownGer, PosList2} ->
			%% 两个武将交换装备
			role_data:replace_equip(GerID,GerTypeID,GerID2),
			UpGer3 = role_crystal:refresh_gercrystal(UpGer),
			UpGer2 = ger_attr:recacl(UpGer3#gerSimple{gerPos=GerPos}, PosList2),
			PosList3 = [UpGer2|PosList2],
			if GerTypeID2 == GerTypeID ->
				   PosList4 = PosList3;
			   true ->
				   %% 重算天命
				   PosList4 = [UpGer2 | [destiny_impact(E,GerTypeID, GerTypeID2, PosList3)||E<-PosList2]]
			end,
      %%此处将需要将下阵精灵生活身上的技能宝石全部卸下放入背包
      DownGer2 = role_diamond:demount_all_diamond_for_ger(DownGer),
			GerBag3 = [ger_lib:ger2gerSimple(DownGer2)|GerBag2],
			ger_lib:notify_update(DownGer2)
	end,  
  role_data:set_gerBag(GerBag3),
	ger_lib:notify_update(UpGer2),
	PosList5 = ger_attr:refresh_fightPower(PosList4),
	role_data:set_posList(PosList5),
  %%触发上阵精灵变化
  role_payGuide:trigger_formation_ger_change(GerID),
	?sendself(#sc_ger_standup{gerPos=GerPos, gerID=GerID, result=1}).

%% 精灵下阵
do_unload(GerPos, #ger{gerID=GerID}=UnLoadGer, PosList2) ->
    %% 回收装备、符文
    EquipList = role_data:get_equip(GerID),
    EquipList2 = lists:map(fun(Item) -> Item#item{itemPos=0, addAttr=0} end, EquipList),
    role_data:del_equip(GerID),
    role_data:set_bagEquip(EquipList2 ++ role_data:get_bagEquip()),

    %%此处将需要将下阵精灵生活身上的技能宝石全部卸下放入背包
    UnLoadGer2 = role_diamond:demount_all_diamond_for_ger(UnLoadGer),
    
    %% 将下阵的放到精灵背包中去
    GerBag2 = [ger_lib:ger2gerSimple(UnLoadGer2)|role_data:get_gerBag()],
    role_data:set_gerBag(GerBag2),

    %% 设置新列表,并且刷新属性
    role_data:set_posList2(PosList2),
    ger_attr:recacl_gers(),
    %%触发上阵精灵变化统计
    role_payGuide:trigger_formation_ger_change(GerID),
    %%触发上阵精灵装备变化统计
    case EquipList of []->ignore;_->role_payGuide:trigger_formation_ger_equip_change(GerID) end,
    ?sendself(#sc_ger_unload{result=1, gerPos=GerPos}).
    
%% 计算受影响的出站武将的属性
destiny_impact(Ger,GerTypeID, GerTypeID2,PosList) ->
	#ger{gerID=GerID,gerBase=#gerBase{gerTypeID=T}} = Ger,
	%% 根据天命配置判断是否需要计算此武将属性
	case lists:any(fun(E) -> E=:=GerTypeID orelse E =:= GerTypeID2 end, data_destiny_rela:get(T)) of
		true ->
			Num = active_destiny_nun(GerID,T,PosList,false),
			?CATCH(role_task_trigger:handle({dispach_task,role_active_destiny_num,Num})),
			Ger2 = ger_attr:recacl(Ger,PosList),
			ger_lib:notify_update(Ger2),
			Ger2;
		false ->
			Ger
	end.
%%计算激活的天命条数
active_destiny_nun(GerID,GerTypeID,PosList,IsLieu)->
	EquipList = role_data:get_equip(GerID),
	SpLieuList=
		case IsLieu of
			true->
				ger_attr:get_main_ger_typeIDList();
			false->
				ger_attr:get_sp_lieu_typeIDList(role_data:get_lieutenantInfo())
		end,
	PosTypeIDList =
		lists:map(fun(#ger{gerBase=#gerBase{gerTypeID=E}}) ->
						  E;
					 (#gerSimple{gerTypeID=E}) ->
						  E
				  end, PosList),
	ArmedGerTypeIDList = lists:delete(GerTypeID, SpLieuList++PosTypeIDList),
	EquipTypeIDList = [ItemTypeID || #item{itemTypeID=ItemTypeID} <- EquipList],
	#data_ger{destinyIDList=DesIDList} = data_ger:get(GerTypeID),
	lists:foldl(fun(DesID,C)->
						#data_destiny{destinyType=DesType, destinyNeedList=DesNeedList} = data_destiny:get(DesID),
						case ger_attr:check_destiny_add(DesType, ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) of
							true->
								C+1;
							false->
								C
						end
				end, 0, DesIDList).
	
%% 移动武将位置
cs_ger_move_pos(#cs_ger_move_pos{gerPos=GerPos,targetPos=TargetPos}) ->
	case check_move_pos(GerPos, TargetPos) of
		{true, ChangedList,OtherList} ->
            %由于天赋加成不同，变换位置后需要重新计算add_attr
            ChangedList2 = [ger_attr:recacl(E, lists:delete(E, ChangedList)++OtherList)||E<-ChangedList],
            PosList2 = ChangedList2++OtherList,
			?sendself(#sc_ger_move_pos{gerPos=GerPos,targetPos=TargetPos,result=1}),
			PosList3 = ger_attr:refresh_fightPower(PosList2),
			role_data:set_posList(PosList3),
			?CATCH(role_task_trigger:handle({dispach_task,role_ger_move_pos_times}));
		{false,Reason} ->
			?sendself(#sc_ger_move_pos{gerPos=GerPos,targetPos=TargetPos,result=Reason})
			end.

%% @doc 出售武将
cs_ger_sell(#cs_ger_sell{gerIDList=GerIDList}) ->
	case check_sell(GerIDList) of
		{false, Reason} ->
			?sendself(#sc_ger_sell{result=Reason,reward=[]});
		{GerBag2, InfoAcc} ->
			do_ger_sell(GerBag2, GerIDList, InfoAcc)
			end.

%% @doc 获取武将详细信息
cs_ger_detail(#cs_ger_detail{gerID=GerID}) ->
    case role_data:get_ger(GerID) of
		  {value, #ger{},_,_,_,ger} ->
			  #ger{gerAttr=GerAttr,gerBase=#gerBase{gerTypeID=GerTypeID}} = take_ger2(GerID);
	    {value, #ger{gerAttr=GerAttr,gerBase=#gerBase{gerTypeID=GerTypeID}},_,_,_,_} -> 
            next;
      {value, #gerSimple{gerTypeID=GerTypeID}=Ger,_,_,_,_} ->
            #ger{gerAttr=GerAttr} = ger_attr:recacl_bag(Ger);
      _ ->
            GerAttr = #gerAttr{},
            GerTypeID = 0
    end,
  %%精灵的速度成长值不会变化，此处直接从配置文件中读出,此处由于配置文件里面的系数存在小数的情况，此处所有的值扩大1000倍，前端需要执行除以1000
  case data_ger:get(GerTypeID) of 
	   ?undefined->
        InitSpeedGrow = 0;
      #data_ger{addSpeed=InitSpeedGrow1}->
        InitSpeedGrow = trunc(InitSpeedGrow1*1000)
  end,
  Reply = #sc_ger_detail{
						   gerID=GerID,
						   gerSpInit		=  GerAttr#gerAttr.gerSpInit		,
						   gerSpMax        =  GerAttr#gerAttr.gerSpMax        ,
						   gerCritic        =  GerAttr#gerAttr.gerCritic        ,
						   gerCriticReduce  =  GerAttr#gerAttr.gerCriticReduce  ,
						   gerDoom          =  GerAttr#gerAttr.gerDoom          ,
						   gerMiss          =  GerAttr#gerAttr.gerMiss          ,
						   gerAbsorb        =  GerAttr#gerAttr.gerAbsorb        ,
						   gerDamageBack    =  GerAttr#gerAttr.gerDamageBack    ,
						   gerReel          =  GerAttr#gerAttr.gerReel          ,
						   gerReelReduce    =  GerAttr#gerAttr.gerReelReduce    ,
						   gerPhyDefBite    =  GerAttr#gerAttr.gerPhyDefBite    ,
						   gerPhyDef        =  GerAttr#gerAttr.gerPhyDef        ,
						   gerMagDefBite    =  GerAttr#gerAttr.gerMagDefBite    ,
						   gerMagDef        =  GerAttr#gerAttr.gerMagDef        ,
               gerSpeed         =  GerAttr#gerAttr.gerSpeed         ,
               gerBaseSpeedGrow =  InitSpeedGrow
						  },
	?sendself(Reply).
						  
						  
						  
cs_ger_lieu_pos_list(_)->
	Starttime = data_partner:get(start_time),
	NowSec = util:seconds_to_datetime(util:now()),
	case NowSec < Starttime of
		true ->  %% 活动未开启
			IsStart = 0;
		false ->  %% 活动开启
			IsStart = 1
	end,
	PosList = [begin
				   EquipList=[Equip#item.itemUID || Equip <-role_data:get_equip(GerID)],				   
				   #p_ger_pos_info{gerID=GerID,gerPos=Pos,itemUIDList=EquipList,diamondList=[role_diamond:to_p_diamond(E)||E<-GerHolyGrailInfo#holyGrail.diamondInfo]}
			   end ||#ger{gerBase=#gerBase{gerPos=Pos,gerHolyGrailInfo=GerHolyGrailInfo},gerID=GerID}<-role_data:get_lieuposList()],
	Reply = #sc_ger_lieu_pos_list{gerPosInfoList=PosList, isStart = IsStart},
	?sendself(Reply).
						  
cs_ger_lieu_standup(#cs_ger_lieu_standup{gerPos=Pos,gerID=ID})->
		case check_lieu_standup(Pos, ID) of
		{true, UpGer, PosList, GerBag2} ->
			do_lieu_standup(Pos, ID, UpGer, PosList, GerBag2);	%% 欲装备的位置，装备的小伙伴ID，小伙伴详细信息，PosList, GerBag2
		{false,Reason} ->
			?sendself(#sc_ger_lieu_standup{gerPos=Pos,gerID=ID, result=Reason})
			end.
						  
cs_ger_lieu_dequeue(#cs_ger_lieu_dequeue{gerPos=Pos,gerID=ID})->
		case check_lieu_dequeue(Pos, ID) of
			{true, UpGer, LPosList2, GerBag} ->
				do_lieu_dequeue(Pos, ID, UpGer, LPosList2, GerBag);	%% 欲卸载的位置，装备的小伙伴ID，小伙伴详细信息，PosList, GerBag2
			{false,Reason} ->
				?sendself(#sc_ger_lieu_dequeue{gerPos=Pos,gerID=ID, result=Reason})
		end.

cs_ger_lieu_move_pos(#cs_ger_lieu_move_pos{gerPos=GerPos,targetPos=TargetPos})->
		case check_lieu_move_pos(GerPos, TargetPos) of
		{true, PosList2} ->
			?sendself(#sc_ger_lieu_move_pos{gerPos=GerPos,targetPos=TargetPos,result=1}),
			%PosList3 = ger_attr:refresh_fightPower(PosList2),
			%role_data:set_lieuposList(PosList2);
			role_data:init_lieuList(PosList2);
		{false,Reason} ->
			?sendself(#sc_ger_lieu_move_pos{gerPos=GerPos,targetPos=TargetPos,result=Reason})
			end.

cs_ger_lieu_untie(#cs_ger_lieu_untie{gerPos=GerPos})->
	case check_untie(GerPos) of
		{false, Reason} ->
			?sendself(#sc_ger_lieu_untie{result=Reason,info=[]});
		{true, Role, InitInfo,Cost} ->
			do_untie(Role,InitInfo,GerPos,Cost);
		_ ->
			?sendself(#sc_ger_lieu_untie{result=4,info=[]})
	end.

cs_ger_lieu_info_list(_)->
	InfoList=
		lists:foldl(fun(#t_lieu{pos=Pos,infoID1=InfoID1,isLock1=IsLock1,infoID2=InfoID2,isLock2=IsLock2,infoID3=InfoID3,isLock3=IsLock3},Acc)->
							[#p_ger_lieu_info{gerPos=Pos,specialID=InfoID1,isLock1=IsLock1,attAddID=InfoID2,isLock2=IsLock2,hpAddID=InfoID3,isLock3=IsLock3}|Acc]
							end,[],role_data:get_lieutenantInfo()),
	?sendself(#sc_ger_lieu_info_list{info=InfoList}).

cs_ger_lieu_lock_clo(#cs_ger_lieu_lock_clo{gerPos=Pos, num=Num})->
	case check_lieu_lock(Pos, Num) of
		{false, Reason}->
			?sendself(#sc_ger_lieu_lock_clo{result=Reason});
		{true , NewLieuInfo,LieuAcc}->
			do_lieu_lock(NewLieuInfo,LieuAcc)
	end.

cs_ger_lieu_unlock_clo(#cs_ger_lieu_unlock_clo{gerPos=Pos, num=Num})->
	case check_lieu_unlock(Pos, Num) of
		{false, Reason} ->
			?sendself(#sc_ger_lieu_unlock_clo{result=Reason});
		{true, NewInfo, LieuAcc} ->
			role_data:set_lieutenantInfo([NewInfo|LieuAcc]),
			?sendself(#sc_ger_lieu_unlock_clo{result=1})
	end.

cs_ger_lieu_refresh_clo(#cs_ger_lieu_refresh_clo{gerPos=Pos})->
	case check_lieu_refresh(Pos) of
		{false, Reason} ->
			PInfo=#p_ger_lieu_info{gerPos=0, specialID=0, isLock1=0, attAddID=0, isLock2=0, hpAddID=0, isLock3=0},
			?sendself(#sc_ger_lieu_refresh_clo{result=Reason,info=PInfo});
		{true,LieuInfo, BagOther2, Role, NeedGold, DelAcc, UpdateAcc, UpdateLogAcc,LieuAcc,RefreshTimes}->
			role_data:set_bagItem(BagOther2),
			Role2 = role_lib:deduct_gold_f(Role,NeedGold, ?MONEY_DEC_TYPE_LIEU_REFRESH, 0,""),
			do_lieu_refresh(Role2, LieuInfo, DelAcc, UpdateAcc, UpdateLogAcc, LieuAcc,RefreshTimes)
	end.

cs_ger_lieu_tie_info(_)->
	LieuInfo = role_data:get_lieutenantInfo(),
	Result = lists:foldl(fun(#t_lieu{pos=Pos},Acc)->
								 [Pos|Acc]
						 end, [],LieuInfo),
	?sendself(#sc_ger_lieu_tie_info{posList=Result}).

cs_ger_lieu_refresh_freeTimes(_)->
	RoleTimes = role_data:get_roleTimes(),
	ReFreshTimes = RoleTimes#roleTimes.refreshLieuTimes,
	if ReFreshTimes > 0 ->
		   ?sendself(#sc_ger_lieu_refresh_freeTimes{times=ReFreshTimes});
	   true ->
		   ?sendself(#sc_ger_lieu_refresh_freeTimes{times=0})
	end.

cs_ger_mirror_info(_)->
    NeedGold = data_common:get(mirror_gold),
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,NewGerTypeId}} = role_data:get_mirror(),
    CurGerUid = GerSimpleInfo#gerSimple.gerID,
    % GerBag = role_data:get_gerBag(),
    ?sendself(#sc_ger_mirror_info{srcGerTypeID=CurGerUid,newGerTypeID=NewGerTypeId,need_gold=NeedGold}).

cs_ger_mirror_convert(#cs_ger_mirror_convert{gerID=GerID})->
    Role = role_data:get_roleInfo(),
    case check_mirror_convert(Role,GerID) of
        {true,NeedGold,MirrorInfo,NewGerSimpleInfo,OtherGerBagList}->
            % 消耗日志在实际替换时记录
            {NewGerTypeID,NewMirrorInfo} = mirror_convert(NewGerSimpleInfo,MirrorInfo),
            role_data:set_mirror(NewMirrorInfo),
            role_data:set_gerBag(OtherGerBagList),  %把这个精灵保存在魔镜数据中
            role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_MIRROR, NewGerTypeID, ""),
            ?sendself(#sc_ger_mirror_convert{result=1,newGerTypeID=NewGerTypeID});
        {false,Reason}->
            ?sendself(#sc_ger_mirror_convert{result=Reason,newGerTypeID=0})
    end.

cs_ger_mirror_commit(#cs_ger_mirror_commit{operation_type=OperationType})->
    case check_mirror_commit() of
        ok ->
            #gerMirrorInfo{gerMirrorState={GerSimpleInfo,NewGerTypeId}} = MirrorInfo =role_data:get_mirror(),
            NewGerUID = case OperationType of
                1 ->
                    %% 写武将消耗日志和升品日志
                    {Date, _} = Time = erlang:localtime(),
                    #gerSimple{gerID=GerID,gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality} = GerSimpleInfo,
                    erlang:send(self(), {ger_del_cancel_cirrus, GerID}),
                    behavior_ger_consume:log(role_data:get_roleID(), [[GerID, GerTypeID, GerLevel, GerQuality]], Date, Time, ?MONEY_DEC_TYPE_MIRROR, NewGerTypeId, ""),
                    ?sendself(#sc_ger_del{gerIDList=[GerID]}),
                    AddGerList = [#new_ger{gerLevel=GerLevel,gerQuality=GerQuality,gerTypeID=NewGerTypeId}],
                    [R] = role_reward:handle_ger_f(AddGerList,  ?MONEY_ADD_TYPE_MIRROR_GER, 0, ""),
                    NewGerUid = R#gerSimple.gerID,
					%role_payGuide:update_uprank(NewGerTypeId,GerQuality),
                    ?INFO("cs_ger_mirror_commit ~w,~w,~w",[R,NewGerTypeId,GerQuality]),
                    {value, {_OldGerID,CurTime}, TupleList2} = lists:keytake(GerID, 1, MirrorInfo#gerMirrorInfo.gerMirrorTimeList),
                    role_data:set_mirror(MirrorInfo#gerMirrorInfo{gerMirrorState = {#gerSimple{gerID=0},0},gerMirrorTimeList=[{NewGerUid,CurTime}|TupleList2]}),
                    NewGerUid;
                2 ->
                    GerBagList = role_data:get_gerBag(),
                    role_data:set_gerBag([GerSimpleInfo|GerBagList]),
                    role_data:set_mirror(MirrorInfo#gerMirrorInfo{gerMirrorState = {#gerSimple{gerID=0},0}}),
                    0
            end,
            ?sendself(#sc_ger_mirror_commit{result=OperationType,newGerUID=NewGerUID});
        {false,Reason} ->
            ?sendself(#sc_ger_mirror_commit{result=Reason,newGerUID=0})
    end.

cs_ger_second_uprank(#cs_ger_second_uprank{srcGerID=SrcGerID,foodGerIDList=FoodGerIDList})->
  case do_cs_ger_second_uprank(SrcGerID,FoodGerIDList) of
    {true,_NewGer}->
      ?sendself(#sc_ger_second_uprank{result=1,srcGerID=SrcGerID});
    {false,Reason}->
      ?sendself(#sc_ger_second_uprank{result=Reason,srcGerID=SrcGerID})
  end.
cs_ger_second_transmigration(#cs_ger_second_transmigration{srcGerID=SrcGerID})->
  case do_cs_ger_second_transmigration(SrcGerID) of
    {true,_NewGer}->
      ?sendself(#sc_ger_second_transmigration{result=1});
    {false,Reason}->
      ?sendself(#sc_ger_second_transmigration{result=Reason})
  end.

do_cs_ger_second_transmigration(SrcGerID)->
  case role_data:get_ger(SrcGerID) of
    {value,Ger,PosList2,LPosList2,GerBag2,Type}->
      case do_cs_ger_second_transmigration2(Ger) of
        {true,NewGer}->
          {GerTypeID,NewGerQuality,GerExp,GerLevel} = case Type of
            ger->
              NewGer1 = role_crystal:refresh_gercrystal(NewGer),
              NewGer2 = ger_attr:recacl(NewGer1,PosList2),        
              PosList3 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
              role_data:set_posList(PosList3),
              {NewGer#ger.gerBase#gerBase.gerTypeID,NewGer#ger.gerBase#gerBase.gerQuality,NewGer#ger.gerBase#gerBase.gerExp,NewGer#ger.gerBase#gerBase.gerLevel};
            lieu->
              ger_lib:notify_update(NewGer),
              %%设置小伙伴的时候，没有推送小伙伴的更新
              role_data:set_lieuposList([NewGer|LPosList2]),
              {NewGer#ger.gerBase#gerBase.gerTypeID,NewGer#ger.gerBase#gerBase.gerQuality,NewGer#ger.gerBase#gerBase.gerExp,NewGer#ger.gerBase#gerBase.gerLevel};
            bag->
              ger_lib:notify_update(NewGer),
              role_data:set_gerBag([NewGer|GerBag2]),
              {NewGer#gerSimple.gerTypeID,NewGer#gerSimple.gerQuality,NewGer#gerSimple.gerExp,NewGer#gerSimple.gerLevel}
          end,
          {Date, _} = Time = erlang:localtime(),
          #role{roleName=RoleName,roleID=RoleID} = role_data:get_roleInfo(),
          role_gather:hook_add_ger_list([{GerTypeID,NewGerQuality}]),
          %%添加升品日志
          behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, GerLevel, GerExp, NewGerQuality-1, GerLevel, GerExp, NewGerQuality, 0, Date, Time),
          #data_ger{gerStar=GerStarLevel}=data_ger:get(GerTypeID),
          role_lvlSgAttr:on_ger_lvl_up(GerStarLevel, GerLevel, GerLevel),
          role_lvlSgAttr:on_ger_rank_up(GerStarLevel, NewGerQuality-1, NewGerQuality),
          case GerStarLevel >= 5 of
            true ->
              broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(NewGer)});
            _ ->
              next
          end,
          {true,NewGer};
        Msg->
          Msg
      end;
    _->
      {false,2}
  end.

do_cs_ger_second_transmigration2(Ger)->
  {GerTypeID,GerQuality,GerID,NewGer,NewGerQuality} = case is_record(Ger,ger) of
    true->
      #ger{gerBase=GerBase,gerID=GerID1} = Ger,
      #gerBase{gerQuality=GerQuality1,gerTypeID=GerTypeID1} = GerBase,
	    NewGerQuality1 = GerQuality1+1,
      NewGerBase = GerBase#gerBase{gerQuality=NewGerQuality1},
      {GerTypeID1,GerQuality1,GerID1,Ger#ger{gerBase=NewGerBase},NewGerQuality1};
    _->
      #gerSimple{gerID=GerID1,gerQuality=GerQuality1,gerTypeID=GerTypeID1} = Ger,
	    NewGerQuality1 = GerQuality1+1,
      {GerTypeID1,GerQuality1,GerID1,Ger#gerSimple{gerQuality=NewGerQuality1},NewGerQuality1}
  end,
  case check_ger_transmigration(GerTypeID,GerQuality) of
    {true,Cost}->
      case check_ger_transmigration_cost(Cost) of
        {BagItem2, DelList, UpdateList, UpdateLogList,NeedCoin,NeedGold, NeedRepu}->
          #role{roleID=RoleID} = Role = role_data:get_roleInfo(),
          role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, ""),
          role_lib:deduct_reputation_f(Role, NeedRepu, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, ""),
          role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, ""),
          %% 写道具日志
          {Date, _} = Time = erlang:localtime(),
          LogItemList = role_item:itemList2logItemList(DelList, UpdateLogList),
          behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 1, integer_to_list(GerID)),
          role_data:set_bagItem(BagItem2),
          DelItemIDList = [E||#item{itemUID=E}<-DelList],
          UpdateList2 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateList],
          %%触发转生成就
          if 
            NewGerQuality == 30 -> 
              ?CATCH(role_task_trigger:handle({dispach_task,ger_up_quality,GerID,GerTypeID,NewGerQuality,transform3}));
            true -> ignore 
          end,
          %% 提醒客户端更新物品
          ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
          ?sendself(#sc_item_update{updateList=UpdateList2}),
          {true,NewGer};
        false->
          {false,5}
      end;
    Msg->
      Msg
  end.

check_ger_transmigration(GerTypeID,GerQuality)->
  case GerQuality >= ?MIN_GER_SECOND_RANK andalso GerQuality =< ?MAX_GER_SECOND_RANK of
    true->
      #data_ger{breakthroughIDList=BreakThroughIDList} = data_ger:get(GerTypeID),
      QualityLimitList = get_breakthrough_Limit(BreakThroughIDList),
      case lists:keyfind(GerQuality, #data_ger_breakthrough.rank_condition,QualityLimitList) of
        false->
          {false,3};
        Find->
          {true,Find#data_ger_breakthrough.condition}
      end;
    false->
      {false,3}
  end.

check_ger_transmigration_cost(Cost)->
  Role = role_data:get_roleInfo(),
  BagItem = role_data:get_bagItem(),
  check_need_list(Role, Cost, BagItem).

cs_ger_batch_uprank(#cs_ger_batch_uprank{srcGerID=SrcGerID,foodGerIDList=FoodGerIDList})->
  case do_cs_ger_batch_uprank(SrcGerID,FoodGerIDList) of
    true->
      ?sendself(#sc_ger_batch_uprank{result=1,srcGerID=SrcGerID});
    {false,Reason}->
      ?sendself(#sc_ger_batch_uprank{result=Reason,srcGerID=SrcGerID})
  end.

do_cs_ger_batch_uprank(SrcGerID,FoodGerIDList)->
  case role_data:get_ger(SrcGerID) of
    {value,Ger,PosList2,LPosList2,GerBag2,Type}->
      case do_cs_ger_batch_uprank2(Ger,FoodGerIDList,GerBag2) of
        {true,NewGerQuality,NewGerBag,FoodExp}->
          Role=role_data:get_roleInfo(),
          {GerTypeID,NewGerQuality,OldGerQuality,OldGerExp,GerExp,OldGerLevel,GerLevel} = case Type of
            ger->
              role_data:set_gerBag(NewGerBag),
              #gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerQuality,gerTypeID=BGerTypeID} = Ger#ger.gerBase,
              {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,BGerTypeID),
              NewGerBase = Ger#ger.gerBase#gerBase{gerQuality=NewGerQuality,gerLevel=Level2,gerExp=Exp2},
              NewGer = Ger#ger{gerBase=NewGerBase},
              NewGer1 = role_crystal:refresh_gercrystal(NewGer),
              NewGer2 = ger_attr:recacl(NewGer1,PosList2),        
              PosList3 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
              role_data:set_posList(PosList3),
              {NewGer#ger.gerBase#gerBase.gerTypeID,NewGer#ger.gerBase#gerBase.gerQuality,SrcGerQuality,SrcGerExp,NewGer#ger.gerBase#gerBase.gerExp,SrcGerLevel,NewGer#ger.gerBase#gerBase.gerLevel};
            lieu->
              role_data:set_gerBag(NewGerBag),
              #gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerQuality,gerTypeID=BGerTypeID} = Ger#ger.gerBase,
              {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,BGerTypeID),
              NewGerBase = Ger#ger.gerBase#gerBase{gerQuality=NewGerQuality,gerLevel=Level2,gerExp=Exp2},
              NewGer = Ger#ger{gerBase=NewGerBase},
              ger_lib:notify_update(NewGer),
              role_data:set_lieuposList([NewGer|LPosList2]),
              {NewGer#ger.gerBase#gerBase.gerTypeID,NewGer#ger.gerBase#gerBase.gerQuality,SrcGerQuality,SrcGerExp,NewGer#ger.gerBase#gerBase.gerExp,SrcGerLevel,NewGer#ger.gerBase#gerBase.gerLevel};
            bag->
              #gerSimple{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerQuality=SrcGerQuality,gerTypeID=BGerTypeID} = Ger,
              {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,BGerTypeID),
              NewGer=Ger#gerSimple{gerQuality=NewGerQuality,gerLevel=Level2,gerExp=Exp2},
              role_data:set_gerBag([NewGer|NewGerBag]),
              ger_lib:notify_update(NewGer),
              {NewGer#gerSimple.gerTypeID,NewGer#gerSimple.gerQuality,SrcGerQuality,SrcGerExp,NewGer#gerSimple.gerExp,SrcGerLevel,NewGer#gerSimple.gerLevel}
          end,
          {Date, _} = Time = erlang:localtime(),
          #role{roleName=RoleName,roleID=RoleID} = role_data:get_roleInfo(),
          role_gather:hook_add_ger_list([{GerTypeID,NewGerQuality}]),
          behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, OldGerLevel, OldGerExp, OldGerQuality, GerLevel, GerExp, NewGerQuality, 0, Date, Time),
          #data_ger{gerStar=GerStarLevel}=data_ger:get(GerTypeID),
          role_lvlSgAttr:on_ger_lvl_up(GerStarLevel, OldGerLevel, GerLevel),
          role_lvlSgAttr:on_ger_rank_up(GerStarLevel, OldGerQuality, NewGerQuality),
          ?CATCH(role_task_trigger:handle({dispach_task,ger_up_quality,SrcGerID,GerTypeID,NewGerQuality,evolution})),
          case GerStarLevel >= 5 of
            true ->
              broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(NewGer)});
            _ ->
              next
          end,
          true;
        {false,Reason}->
          {false,Reason}
      end;
    _->
      {false,2}
  end.

%%完成实际的升品过程，返回{true，NewGer,GerBag}｜{false,Reason}
do_cs_ger_batch_uprank2(Ger,FoodGerIDList,GerBag)->
  {GerTypeID,GerQuality,GerID} = if
  is_record(Ger,ger) ->
    #ger{gerBase=GerBase,gerID=GerID1} = Ger,
    #gerBase{gerTypeID=GerTypeID1,gerQuality=GerQuality1} = GerBase,
    {GerTypeID1,GerQuality1,GerID1};
  true->
    #gerSimple{gerTypeID=GerTypeID1,gerQuality=GerQuality1,gerID=GerID1} = Ger,
    {GerTypeID1,GerQuality1,GerID1}
  end,
  case check_ger_uprank_condition(GerTypeID,GerQuality) of
    {true,NeedQuality}->
      #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
      case check_foodger(NeedQuality,FoodGerIDList,GerStar,GerBag) of
        {true,RealIncQuality,RealFoodGerList,NewGerBag,FoodExp}->
          CoinCost = get_batch_up_rank_coin(RealIncQuality,GerQuality,RealFoodGerList,GerStar,GerTypeID),
          #role{coin=Coin,roleID=RoleID}=Role = role_data:get_roleInfo(),
          case Coin >= CoinCost of
            true->
              %%添加金币消耗日志
              Role2=role_lib:deduct_coin_f(Role, CoinCost, ?MONEY_DEC_TYPE_BATCH_UPRANK, 0, integer_to_list(GerID)),
              role_data:set_roleInfo(Role2),
              %%添加精灵消耗日志
              LogGerList = role_ger:gerList2logGerList(RealFoodGerList),
              {Date, _} = Time = erlang:localtime(),
              [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
              DeleteGerIDList = [ID||#gerSimple{gerID = ID}<-RealFoodGerList],
              ?sendself(#sc_ger_del{gerIDList=DeleteGerIDList}),
              behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_BATCH_UPRANK, 0, integer_to_list(GerID)),
              {true,RealIncQuality+GerQuality,NewGerBag,FoodExp};
            false->
              {false,7}
          end;
        {false,Reason}->
          {false,Reason}
      end;
    {false,Reason}->
      {false,Reason}
  end.

%%判断精灵能够进化，并返回能够一次性进化到多少阶
check_ger_uprank_condition(GerTypeID,GerQuality)->
  case GerQuality <?MAX_GER_RANK of
    true->
      #data_ger{breakthroughIDList=BreakThroughIDList} = data_ger:get(GerTypeID),
      QualityLimitList = get_breakthrough_Limit(BreakThroughIDList),
      case lists:keyfind(GerQuality,#data_ger_breakthrough.rank_condition,QualityLimitList) of
        false->
          if
            GerQuality <10 ->
              {true,9-GerQuality};
            GerQuality <20 ->
              {true,19-GerQuality};
            true->
              {false,3}
           end;
        _Find->
          {false,4}
      end;
    false->
      {false,3}
  end.

%%返回批量进化需要的金币消耗
get_batch_up_rank_coin(RealIncQuality,GerQuality,RealFoodGerList,GerStar,GerTypeID)->
  AllFoodGerCoin = lists:sum([get_up_rank_coin(Ger#gerSimple.gerTypeID,Ger#gerSimple.gerQuality,GerStar)||Ger<-RealFoodGerList]),
  DesGerCoin = get_up_rank_coin(GerTypeID,RealIncQuality+GerQuality,GerStar),
  SrcGerCoin = get_up_rank_coin(GerTypeID,GerQuality,GerStar),
  max(0,DesGerCoin-SrcGerCoin-AllFoodGerCoin).

%%判断批量升品需要消耗的精灵以及能够升品多少阶
check_foodger(IncQuality,FoodGerIDList,GerStar,GerBag)->
  Role = role_data:get_roleInfo(),
  Msg = util:foldl(fun(GerID,{Acc,RemainAcc})->
    case lists:keytake(GerID,#gerSimple.gerID,RemainAcc) of
      {value,Find,Other}->
        #data_ger{gerStar=FoodGerStar} = data_ger:get(Find#gerSimple.gerTypeID),
        case FoodGerStar=:=GerStar of
          true->
            case homestead_server:has_homestead_ger(Role#role.roleID,[GerID]) of
              true->
                {return,{false,8}};
              false->
                case is_mirror_ger(GerID) of
                  true->
                    {return,{false,9}};
                  false->
                    {[Find|Acc],Other}
                end
            end;
          false->
            {return,{false,6}}
        end;
      false->
        {return,{false,5}}
    end
  end,{[],GerBag},FoodGerIDList),
  case Msg of
    {false,Reason}->
      {false,Reason};
    {[],_RemainGerAcc}->
      {false,10};
    {FindGerAcc,RemainGerAcc} when is_list(FindGerAcc) andalso is_list(RemainGerAcc)->
      {RealFoodGerList,RealIncQuality,LastRemainAcc,ExpAcc} = get_suit_food_ger(FindGerAcc,IncQuality),
      {true,min(RealIncQuality,IncQuality),RealFoodGerList,RemainGerAcc++LastRemainAcc,ExpAcc};
    _->
      {false,5}
  end.

get_suit_food_ger(FoodGerList,GerQuality)->
  SortFoodGerList = lists:sort(fun(A,B)->A#gerSimple.gerQuality<B#gerSimple.gerQuality end,FoodGerList),
  lists:foldl(fun(Ger,{Acc,Count,RemainAcc,ExpAcc})->
    case Count < GerQuality of
      true->
        {[Ger|Acc],Count+Ger#gerSimple.gerQuality+1,RemainAcc,ExpAcc+Ger#gerSimple.gerExp};
      false->
        {Acc,Count,[Ger|RemainAcc],ExpAcc}
    end
  end,{[],0,[],0},SortFoodGerList).

do_cs_ger_second_uprank(SrcGerID,FoodGerIDList)->
  case role_data:get_ger(SrcGerID) of
    {value,Ger,PosList2,LPosList2,GerBag2,Type}->
      case do_cs_ger_second_uprank2(Ger,FoodGerIDList,GerBag2) of
        {true,NewGer,NewGerBag}->
          {GerTypeID,NewGerQuality,GerExp,GerLevel,OldGerQuality} = case Type of
            ger->
              role_data:set_gerBag(NewGerBag),
              NewGer1 = role_crystal:refresh_gercrystal(NewGer),
              NewGer2 = ger_attr:recacl(NewGer1,PosList2),       
              PosList3 = ger_attr:refresh_fightPower([NewGer2|PosList2]),
              %%set_posList(PosList)函数中会重新计算精灵的属性并且往前端推送
              role_data:set_posList(PosList3),
              {NewGer#ger.gerBase#gerBase.gerTypeID,NewGer#ger.gerBase#gerBase.gerQuality,NewGer#ger.gerBase#gerBase.gerExp,NewGer#ger.gerBase#gerBase.gerLevel,Ger#ger.gerBase#gerBase.gerQuality};
            lieu->
              role_data:set_gerBag(NewGerBag),
              ger_lib:notify_update(NewGer),
              role_data:set_lieuposList([NewGer|LPosList2]),
              {NewGer#ger.gerBase#gerBase.gerTypeID,NewGer#ger.gerBase#gerBase.gerQuality,NewGer#ger.gerBase#gerBase.gerExp,NewGer#ger.gerBase#gerBase.gerLevel,Ger#ger.gerBase#gerBase.gerQuality};
            bag->
              role_data:set_gerBag([NewGer|NewGerBag]),
              ger_lib:notify_update(NewGer),
              {NewGer#gerSimple.gerTypeID,NewGer#gerSimple.gerQuality,NewGer#gerSimple.gerExp,NewGer#gerSimple.gerLevel,Ger#gerSimple.gerQuality}
          end,
          {Date, _} = Time = erlang:localtime(),
          #role{roleName=RoleName,roleID=RoleID} = role_data:get_roleInfo(),
          role_gather:hook_add_ger_list([{GerTypeID,NewGerQuality}]),
          behavior_ger_uprank:log(RoleID, SrcGerID, GerTypeID, GerLevel, GerExp, NewGerQuality-1, GerLevel, GerExp, NewGerQuality, 0, Date, Time),
          #data_ger{gerStar=GerStarLevel}=data_ger:get(GerTypeID),
          role_lvlSgAttr:on_ger_lvl_up(GerStarLevel, GerLevel, GerLevel),
          % ?ERR("GerStarLevel:~w GerLevel:~w OldGerQuality:~w NewGerQuality:~w ~n",[GerStarLevel,GerLevel,OldGerQuality,NewGerQuality]),
          role_lvlSgAttr:on_ger_rank_up(GerStarLevel, OldGerQuality, NewGerQuality),
          case GerStarLevel >= 5 of
            true ->
              broadcast_server:bc(#sc_message_ger_upLevel{roleName=RoleName,gerInfo=ger_lib:ger2p_ger_view(NewGer)});
            _ ->
              next
          end,
          {true,NewGer};
        Msg->
          Msg
      end;
    _->
      %%此处魔镜状态中的精灵好像不在精灵的背包中
      case is_mirror_ger(SrcGerID) of
        true->
          {false,15};
        false->
          {false,2}
      end
  end.

%%完成实际的升品过程，返回{true，NewGer,GerBag}｜{false,Reason}
do_cs_ger_second_uprank2(Ger,FoodGerIDList,GerBag)->
  {GerTypeID,GerQuality,GerID} = if
  is_record(Ger,ger) ->
    #ger{gerBase=GerBase1,gerID=GerID1} = Ger,
    #gerBase{gerTypeID=GerTypeID1,gerQuality=GerQuality1} = GerBase1,
    {GerTypeID1,GerQuality1,GerID1};
  true->
    #gerSimple{gerTypeID=GerTypeID1,gerQuality=GerQuality1,gerID=GerID1} = Ger,
    {GerTypeID1,GerQuality1,GerID1}
  end,
  case check_second_uprank_condition(GerTypeID,GerQuality) of
    true->
      do_ger_real_uprank(Ger,GerID,GerTypeID,GerQuality,FoodGerIDList,GerBag);
    Msg->
      Msg
  end.

do_ger_real_uprank(Ger,_GerID,GerTypeID,GerQuality,FoodGerIDList,GerBag)->
  case catch check_second_uprank_foodger_condition(FoodGerIDList,GerTypeID,GerQuality,GerBag) of
    {true,NewGerBag,FoodGerList,CoinCost,FoodExp}->
      case check_second_uprank_coin_enough(CoinCost) of
        true->
          add_cs_ger_second_uprank_log(Ger,FoodGerList,CoinCost),
          Role = role_data:get_roleInfo(),
          case is_record(Ger,ger) of
            true->
              #ger{gerBase=GerBase} = Ger,
              #gerBase{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerTypeID=GerTypeID} = GerBase,
              {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,GerTypeID),
              {true,Ger#ger{gerBase=GerBase#gerBase{gerQuality=GerQuality+1,gerLevel=Level2,gerExp=Exp2}},NewGerBag};
            false->
              #gerSimple{gerLevel=SrcGerLevel,gerExp=SrcGerExp,gerTypeID=GerTypeID} = Ger,
              {_, Level2, Exp2} = ger_lib:add_exp(SrcGerLevel,SrcGerExp, FoodExp, Role#role.level,GerTypeID),
              {true,Ger#gerSimple{gerQuality=GerQuality+1,gerLevel=Level2,gerExp=Exp2},NewGerBag}
          end;
        false->
          {false,10}
      end;
    Msg->
      Msg
  end.

do_ger_second_transform(Ger,_GerID,_GerTypeID,GerQuality,GerBag,TransformInfo)->
  #data_ger_breakthrough{condition=NeedList} = TransformInfo,
  Role = role_data:get_roleInfo(),
  BagItem = role_data:get_bagItem(),
  case check_need_list(Role, NeedList, BagItem) of
    false ->
      {false,14}; 
    {BagItem2, DelItem, UpdateItem, UpdateLogList,NeedCoin,NeedGold, NeedRepu} ->
      #role{roleID=RoleID} = Role,
      role_lib:deduct_coin_f(Role, NeedCoin, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, ""),
      role_lib:deduct_reputation_f(Role, NeedRepu, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, ""),
      role_lib:deduct_gold_f(Role, NeedGold, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, ""),
      {Date, _} = Time = erlang:localtime(),
      if 
        DelItem == [] andalso UpdateItem == [] ->
          ignore;
        true ->
          %% 写道具日志
          LogItemList = role_item:itemList2logItemList(DelItem, UpdateLogList),
          behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 1, ""),
          role_data:set_bagItem(BagItem2),
          DelItemIDList = [E||#item{itemUID=E}<-DelItem],
          UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItem],
          %% 提醒客户端更新物品
          ?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList}),
          ?sendself(#sc_item_update{updateList=UpdateList})
      end,
      if
        is_record(Ger,ger)->
          {true,Ger#ger.gerBase#gerBase{gerQuality=GerQuality+1},GerBag};
        true->
          {true,Ger#gerSimple{gerQuality=GerQuality+1},GerBag}
      end
  end.

%%添加升品消耗日志
add_cs_ger_second_uprank_log(Ger,FoodGerList,CoinCost)->
  #role{roleID=RoleID} = Role=role_data:get_roleInfo(),
  %%添加金币消耗日志
  Role2=role_lib:deduct_coin_f(Role, CoinCost, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, integer_to_list(Ger#ger.gerID)),
  role_data:set_roleInfo(Role2),
  %%添加精灵消耗日志
  LogGerList = role_ger:gerList2logGerList(FoodGerList),
  {Date, _} = Time = erlang:localtime(),
  [erlang:send(self(), {ger_del_cancel_cirrus, GID})||[GID,_,_,_]<-LogGerList],
  DeleteGerIDList = [GerID||#gerSimple{gerID = GerID}<-FoodGerList],
  ?sendself(#sc_ger_del{gerIDList=DeleteGerIDList}),
  behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_SECOND_EVOLUTION, 0, integer_to_list(Ger#ger.gerID)).

%%判断对应类型精灵以及品阶是否能够二次进化,进化还是转生
check_second_uprank_condition(GerTypeID,GerQuality)->
  case GerQuality >= ?MAX_GER_SECOND_RANK of
    true->
      {false,4};
    false->
      case GerQuality >= ?MIN_GER_SECOND_RANK of
        true->
          #data_ger{gerSecondEvolutionID=GerSecondEvolutionID,breakthroughIDList=BreakThroughIDList} = data_ger:get(GerTypeID),
          QualityLimitList = get_breakthrough_Limit(BreakThroughIDList),
          case lists:keyfind(GerQuality, #data_ger_breakthrough.rank_condition,QualityLimitList) of
            false->
              case data_second_evolution:get({GerSecondEvolutionID,GerQuality}) of
                ?undefined->
                  {false,3};
                _Cost->
                  true
              end;
            _Find->
              {false,5}
          end;
        false->
          {false,6}
      end
  end.

%%判断被吞噬的精灵是否满足条件
check_second_uprank_foodger_condition(FoodGerIDList,GerTypeID,GerQuality,GerBag)->
  #data_ger{gerSecondEvolutionID=GerSecondEvolutionID,gerStar=GerStar} = data_ger:get(GerTypeID),
  case data_second_evolution:get({GerSecondEvolutionID,GerQuality}) of
    ?undefined->
      {false,8};
    {CoinCost,GerNum}->
      case length(FoodGerIDList)=:= GerNum of
        true->
          {FoodGerList,LastGerBag,FoodExp} = lists:foldl(fun(GerID,{FoodAcc,RemainAcc,ExpAcc})->
            case lists:keytake(GerID,#gerSimple.gerID,RemainAcc) of
              false->
                erlang:throw({false,8});
              {value,Find,Other}->
                case check_single_foodger(Find,GerStar) of
                  true->
                    {[Find|FoodAcc],Other,ExpAcc+Find#gerSimple.gerExp};
                  {false,Reason}->
                    ?ERR("Reason:~w ~n",[Reason]),
                    erlang:throw({false,Reason})
                end
            end end ,{[],GerBag,0},FoodGerIDList),
          {true,LastGerBag,FoodGerList,CoinCost,FoodExp};
        false->
          {false,7}
      end
  end.

%%判断单个精灵是否能够被吞噬
check_single_foodger(Ger,GerStar)->
  #data_ger{gerStar=FoodGerStar} = data_ger:get(Ger#gerSimple.gerTypeID),
  Role = role_data:get_roleInfo(),
  case homestead_server:has_homestead_ger(Role#role.roleID, [Ger#gerSimple.gerID]) of
    true->
      {false,11};
    false->
      case is_mirror_ger(Ger#gerSimple.gerID) of
        true->
          {false,12};
        false->
          if
            Ger#gerSimple.gerQuality=/=0 ->
              {false,9};
            GerStar=/=FoodGerStar->
              {false,13};
            true->
              true
          end
      end
  end.

%%判断玩家的金币是否满足
check_second_uprank_coin_enough(CoinCost)->
  Role = role_data:get_roleInfo(),
  Role#role.coin >= CoinCost.

check_mirror_convert(Role,GerID)->
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_NewGerTypeId}} = MirrorInfo =role_data:get_mirror(),
    CurGerUid = GerSimpleInfo#gerSimple.gerID,
    GerBagList = role_data:get_gerBag(),
    case lists:keytake(GerID, #gerSimple.gerID, GerBagList) of
        %此处由于出现精灵的二次进化，达到20阶以上，所以修改
        {value,NewGerSimpleInfo,OtherGerBagList} when NewGerSimpleInfo#gerSimple.gerQuality >= ?MAX_GER_RANK->
            if
                CurGerUid =/= 0 ->
                    {false,2};
                true ->
                    #data_ger{gerStar=Star} = data_ger:get(NewGerSimpleInfo#gerSimple.gerTypeID),   
                    NeedGold = lists:nth(Star-4, data_common:get(mirror_gold)) ,
                    case role_lib:check_money(Role, gold, NeedGold) of
                        true ->
                            {true,NeedGold,MirrorInfo,NewGerSimpleInfo,OtherGerBagList};
                        false ->
                            {false,3}
                    end
            end;
        {value, #gerSimple{}, _} ->
            {false,5};
        false ->
            {false,4}
    end.

check_mirror_commit()->
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_NewGerTypeId}} = _MirrorInfo =role_data:get_mirror(),
    CurGerUid = GerSimpleInfo#gerSimple.gerID,
    if
        CurGerUid =:= 0 ->
            {false,3};
        true ->
            ok
    end.
    

mirror_convert(GerSimpleInfo,MirrorInfo)->
    #gerMirrorInfo{gerMirrorTimeList=MirrorTimeList} = MirrorInfo,
    GerID = GerSimpleInfo#gerSimple.gerID,
    #data_ger{gerStar=Star} = data_ger:get(GerSimpleInfo#gerSimple.gerTypeID),
    MaxTimes = data_mirror:get({Star,max}),
    {NewTime,NewMirrorTimeList} = case lists:keytake(GerID, 1, MirrorTimeList) of
        {value, Tuple, TupleList2} ->
            Time = case Tuple of
                {GerID,OldTime} when OldTime =:= MaxTimes ->
                    OldTime;
                {GerID,OldTime} when OldTime < MaxTimes ->
                    OldTime+1;
                {GerID,OldTime} when OldTime > MaxTimes ->
                    ?ERR("mirror_convert 逻辑错误 所记录的OldTime大于~w (~w)",[MaxTimes,OldTime]),
                    MaxTimes
            end,
            {Time,[{GerID,Time}|TupleList2]};
        false ->
            {1,[{GerID,1}|MirrorTimeList]}
    end,
    ?INFO("mirror_convert time:~w",[NewTime]),
    NewGerType = random_mirror_ger(Star,NewTime,GerSimpleInfo#gerSimple.gerTypeID),
    {NewGerType,MirrorInfo#gerMirrorInfo{gerMirrorState={GerSimpleInfo,NewGerType},gerMirrorTimeList=NewMirrorTimeList}}.

random_mirror_ger(Star,NewTime,TypeID)->
    RandList0 = data_mirror:get({Star,NewTime}),
    RandList = [E||E<-RandList0,erlang:element(2, E) =/= TypeID],
    util:random_one_from_weigh_list(RandList).
%%     RandList = [E||E<-RandList0,erlang:element(1, erlang:element(2, E)) =/= TypeID],
%%     {Type,_Quality}=util:random_one_from_weigh_list(RandList),
%%     Type.

do_lieu_refresh(#role{roleID=RoleID}, LieuInfo, DelAcc, UpdateAcc, UpdateLogAcc, LieuAcc, RefreshTimes)->
	case UpdateAcc =/= [] of
		true ->
			UpdateList = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateAcc],
			?sendself(#sc_item_update{updateList=UpdateList});
		_ ->
			ignore
	end,
	case DelAcc =/= [] of
		true ->
			DelItemIDList = [E||#item{itemUID=E}<-DelAcc],
			?sendself(#sc_item_delete_notify{itemUIDList=DelItemIDList});
		_ ->
			ignore
	end,
	%%道具消耗,由于可能多次消耗的是相同的道具,所以这里同时产生的消耗应该将最少的结果最为最终结果
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_LIEU_REFRESH, 0, ""),

	case data_fixed_lieu_refresh:get(RefreshTimes) of
		?undefined ->
		   #t_lieu{pos=Pos, infoID1=ID1, infoID2=ID2, infoID3 = ID3, isLock1=LK1, isLock2=LK2, isLock3=LK3} = 
					  NewLieuInfo = get_random_lieu_result(LieuInfo);
	   ValueInfo ->
		   #t_lieu{pos=Pos, infoID1=ID1, infoID2=ID2, infoID3 = ID3, isLock1=LK1, isLock2=LK2, isLock3=LK3} = 
					  NewLieuInfo = get_special_lieu_result(LieuInfo,ValueInfo)
	end,
	role_data:set_lieutenantInfo([NewLieuInfo|LieuAcc]),
	role_data:set_lieuposList(role_data:get_lieuposList()),
	PInfo = #p_ger_lieu_info{gerPos=Pos, specialID=ID1, isLock1=LK1, attAddID=ID2, isLock2=LK2, hpAddID=ID3, isLock3=LK3},
	?sendself(#sc_ger_lieu_refresh_clo{result=1, info=PInfo}).

get_special_lieu_result(#t_lieu{infoID1=ID1,isLock1=LK1,infoID2=ID2,isLock2=LK2,infoID3=ID3,isLock3=LK3}=Info,{S1,S2,S3})->
	ID1N = get_lieu_special_ID(ID1, LK1, S1,special_ger),
	ID2N = get_lieu_special_ID(ID2, LK2, S2,att_add),
	ID3N = get_lieu_special_ID(ID3, LK3, S3,hp_add),
	Info#t_lieu{infoID1=ID1N, infoID2=ID2N, infoID3=ID3N}.

get_lieu_special_ID(ID, LK, List, Type) ->
	if LK =:= 1 ->
		   ID;
	   true ->
		   Random_List = 
			   case Type of
				   special_ger ->
					   lists:foldl(fun(E,Acc)->lists:delete(E, Acc) end, List, get_lieu_spID_list());
				   _ ->
					   List
			   end,
		   if Random_List == [] ->
				  get_lieu_random_ID(ID, LK, Type);
			  true ->
				  
				%  ?ERR("~w",[Random_List]),
				  util:random_one_from_list(Random_List)
		   end
	end.

get_random_lieu_result(#t_lieu{infoID1=ID1,isLock1=LK1,infoID2=ID2,isLock2=LK2,infoID3=ID3,isLock3=LK3}=Info)->
	ID1N = get_lieu_random_ID(ID1, LK1, special_ger),
	ID2N = get_lieu_random_ID(ID2, LK2, att_add),
	ID3N = get_lieu_random_ID(ID3, LK3, hp_add),
	Info#t_lieu{infoID1=ID1N, infoID2=ID2N, infoID3=ID3N}.

get_lieu_random_ID(ID1,LK1,Type)->
	if LK1 =:= 1 ->
		   ID1;
	   true ->
		   Random_List = data_lieu_clo_setting:get(Type),
		   Random_ListT = util:random_one_from_weigh_list(Random_List),
		   Random_List2 = 
			   case Type of
				   special_ger ->
					   lists:foldl(fun(E,Acc)->lists:delete(E, Acc) end, Random_ListT, get_lieu_spID_list());
				   _ ->
					   Random_ListT
			   end,
		   util:random_one_from_list(Random_List2)
	end.

check_lieu_refresh(Pos) ->
	LieuInfo0 = role_data:get_lieutenantInfo(),
	case lists:keytake(Pos, #t_lieu.pos, LieuInfo0) of
		false ->
			{false, 2};
		{value, LieuInfo, LieuAcc}->
			BagOther = role_data:get_bagItem(),
			Role = role_data:get_roleInfo(),
			%{ItemTypeID, ItemNum, GoldUnit} 
			RoleTimes = role_data:get_roleTimes(),
			ReFreshTimes = RoleTimes#roleTimes.refreshLieuTimes,
			if ReFreshTimes > 0 ->
				   role_data:set_roleTimes(RoleTimes#roleTimes{refreshLieuTimes = ReFreshTimes-1}),
				   {true, LieuInfo, BagOther, Role, 0, [], [], [], LieuAcc,0};
			   true ->
				   Refresh_lieu_need = data_lieu_clo_setting:get(refresh_lieu_need),
				   Close_lieu_need = data_lieu_clo_setting:get(get_lieu_clo_count(LieuInfo)),
				   
				   case check_need_material_or_gold(BagOther, Role, [Refresh_lieu_need, Close_lieu_need],{0, [],[],[]}) of
					   {false, Reason} ->
						   {false , Reason};
					   {BagOther2, Role, {NeedGold, DelAcc, UpdateAcc, UpdateLogAcc}} ->
						   PayRefreshLieuTimes = RoleTimes#roleTimes.alreadyPayRefreshLieuTimes,
						   role_data:set_roleTimes(RoleTimes#roleTimes{alreadyPayRefreshLieuTimes=PayRefreshLieuTimes+1}),
						   
						   {true, LieuInfo, BagOther2, Role, NeedGold, DelAcc, lists:reverse(UpdateAcc), UpdateLogAcc, LieuAcc,PayRefreshLieuTimes}
				   end
			end
	end.

check_need_material_or_gold(BagOther, Role, [],Result) ->
	{BagOther, Role,Result};
check_need_material_or_gold(BagOther, Role, [{ItemTypeID, ItemNum,GoldUnit}|T],  {NeedGold, DelAcc, UpdateAcc, UpdateLogAcc})->
	case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
		{BagOther2, 0, DelAcc2, UpdateAcc2, UpdateLogAcc2} ->
			check_need_material_or_gold(BagOther2, Role, T, {NeedGold, DelAcc2++DelAcc, UpdateAcc2++UpdateAcc, UpdateLogAcc2++UpdateLogAcc});
		{BagOther3, RestNum, DelAcc3, UpdateAcc3, UpdateLogAcc3}->
			NeedGold2 = RestNum * GoldUnit + NeedGold,
			case role_lib:check_money(Role, gold, NeedGold2) of
				true ->
					check_need_material_or_gold(BagOther3, Role, T, {NeedGold2, DelAcc3++DelAcc, UpdateAcc3++UpdateAcc, UpdateLogAcc3++UpdateLogAcc});
				false ->
					{false,3}
			end;
		_ ->
			{false, 3}
	end.
					
get_lieu_clo_count(#t_lieu{isLock1=LK1, isLock2=LK2, isLock3=LK3})->
	LK1 + LK2 + LK3.

check_lieu_unlock(Pos, Num)->
	LieuInfo=role_data:get_lieutenantInfo(),
	case lists:keytake(Pos, #t_lieu.pos, LieuInfo) of
		false ->
			{false, 3};
		{value, Info, LieuAcc}->
			case check_lieu_unlock2(Num, Info) of
				{true, NewLieuInfo}->
					{true, NewLieuInfo, LieuAcc};
				Result ->
					Result
			end
	end.

check_lieu_unlock2(Num, {t_lieu,_, _, LK1, _, LK2, _, LK3}=Info)->
	if Num =:= 1, LK1 =:= 1 ->
		   {true, Info#t_lieu{isLock1=0}};
	   Num =:= 2, LK2 =:= 1 ->
		   {true, Info#t_lieu{isLock2=0}};
	   Num =:= 3, LK3 =:= 1 ->
		   {true, Info#t_lieu{isLock3=0}};
	   true ->
		   {false, 2}
	end.


do_lieu_lock(#role{roleID=RoleID}, NewLieuInfo, DelAcc, UpdateLogAcc,LieuAcc)->
	LogItemList = role_item:itemList2logItemList(DelAcc, UpdateLogAcc),
	{Date, _} = Time = erlang:localtime(),
	behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_LIEU_LOCK, 0, ""),

	role_data:set_lieutenantInfo([NewLieuInfo|LieuAcc]),
	?sendself(#sc_ger_lieu_lock_clo{result=1}).
do_lieu_lock(NewLieuInfo, LieuAcc)->
	role_data:set_lieutenantInfo([NewLieuInfo|LieuAcc]),
	?sendself(#sc_ger_lieu_lock_clo{result=1}).
	
check_lieu_lock(Pos, Num)->
	LieuInfo0 = role_data:get_lieutenantInfo(),
	case lists:keytake(Pos, #t_lieu.pos, LieuInfo0) of
		false ->
			{false, 3};
		{value, LieuInfo, LieuAcc} ->
			case check_lieu_lock2(Num, LieuInfo) of
				{true, NewLieuInfo,LKSum}->
					if LKSum >= 0 , LKSum < 3 ->
						   {true, NewLieuInfo,LieuAcc};
					   true ->
						   {false, 5}
					end;
%% 					case data_lieu_clo_setting:get(LKSum+1) of
%% 						{ItemTypeID, ItemNum, GoldUnit} ->
%% 							BagOther = role_data:get_bagItem(),
%% 							case item_lib:check_material2(BagOther, ItemTypeID, ItemNum) of
%% 								{BagOther2, 0, DelAcc, _UpdateAcc, UpdateLogAcc} ->
%% 									{true, NewLieuInfo, BagOther2, DelAcc, UpdateLogAcc,LieuAcc};
%% 								{BagOther2, RestNum, DelAcc, _UpdateAcc, UpdateLogAcc} ->
%% 									NeedGold = RestNum * GoldUnit,
%% 									Role = role_data:get_roleInfo(),
%% 									case role_lib:check_money(Role, gold, NeedGold) of
%% 										true ->
%% 											{true,NewLieuInfo, BagOther2, Role, NeedGold, DelAcc, UpdateLogAcc,LKSum,LieuAcc};
%% 										false ->
%% 											{false, 4}
%% 									end;
%% 								_ ->
%% 									{false, 4}
%% 							end;
%% 						_ ->
%% 							{false, 5}
%% 					end;
				Result ->
					Result
			end
	end.

check_lieu_lock2(Num, {t_lieu,_, _, LK1, _, LK2, _, LK3}=Info)->
	if Num =:= 1, LK1 =:= 0 ->
		   {true, Info#t_lieu{isLock1=1},LK1+LK2+LK3};
	   Num =:= 2, LK2 =:= 0 ->
		   {true, Info#t_lieu{isLock2=1},LK1+LK2+LK3};
	   Num =:= 3, LK3 =:= 0 ->
		   {true, Info#t_lieu{isLock3=1},LK1+LK2+LK3};
	   true ->
		   {false, 2}
	end.

do_untie(_Role, InitInfo,GerPos,_Cost)->
%% 	role_lib:deduct_reputation_f(Role, Cost, ?MONEY_DEC_TYPE_UNTIE_LIEU, 0, ""),   %% 目前解锁不扣除reputation
	{t_lieu,_,ID1,_,ID2,_,ID3,_}=LieuInfoT=get_init_lieutenant(GerPos,InitInfo),
	LieuInfo = role_data:get_lieutenantInfo(),
	role_data:set_lieutenantInfo([LieuInfoT|LieuInfo]),
	Info=#p_ger_lieu_info{gerPos=GerPos,specialID=ID1,attAddID=ID2,hpAddID=ID3,isLock1=0,isLock2=0,isLock3=0},
	?sendself(#sc_ger_lieu_untie{result=1,info=[Info]}).

check_untie(GerPos)->
	LieuInfo = role_data:get_lieutenantInfo(),
	case lists:keyfind(GerPos, #t_lieu.pos, LieuInfo) of
		#t_lieu{} ->
			{false, 4};
		_ ->
			#data_lieu_open_charge{initList=InitInfo}=data_lieu_open_charge:get(GerPos),
			PartnerPositionList=data_partner:get(partner_position_list),
			case lists:keyfind(GerPos, 1, PartnerPositionList)of
				{_,NeedLevel,Cost}->
					#role{level=Level}=role_data:get_roleInfo(),
					if Level < NeedLevel ->
						   {false,2};
					   true ->
						   case check_untie_cost(Cost) of
							   {true, Role}->
								   {true, Role, InitInfo,Cost};
							   Result ->
								   Result
						   end
					end;
				false->
					{false,2}
			end
	end.

%% 不用检测reputation了
check_untie_cost(_Cost)->
	#role{reputation=_Repu} = Role = role_data:get_roleInfo(),
%% 	if Cost > Repu ->
%% 		   {false,3};
%% 	   true ->
		   {true, Role}.
%% 	end.

check_lieu_move_pos(GerPos, TargetPos) ->
	PosList = ger_attr:get_original_lieu_posList(),
	%PosList = role_data:get_lieuposList(),
	case util:fun_take(fun(E) -> GerPos== E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			{false, 3};
		{value, E, PosList2} ->
			case util:fun_take(fun(E1) -> TargetPos== E1#ger.gerBase#gerBase.gerPos end, PosList2) of
				false ->
					PosList4 = [?change_pos(E,TargetPos)|PosList2];
				{value, E2, PosList3} ->
					PosList4 = [?change_pos(E2,GerPos), ?change_pos(E,TargetPos)| PosList3]
			end,
			{true, PosList4}
	end.

% check standup
check_lieu_standup(GerPos, GerID)->
    LieuInfoT = role_data:get_lieutenantInfo(),
    case lists:keyfind(GerPos, #t_lieu.pos, LieuInfoT) of
        #t_lieu{} ->
            case role_ger:is_mirror_ger(GerID) of
                true ->
                    {false, 7};
                false ->
                    GerBag = role_data:get_gerBag(),
                    case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
                        false ->
                            {false, 3};
                        {value, #gerSimple{gerTypeID=GerTypeID}=UpGer, GerBag2} ->
                            case util:is_exp_card(GerTypeID) of
                                false ->
                                    case (is_integer(GerPos) andalso GerPos >0 andalso GerPos =< data_partner:get(partner_num)) of
                                        true ->
                                             #data_ger{sameType=SameType} = data_ger:get(GerTypeID),
                                            LPosList = ger_attr:get_original_lieu_posList(),
                                            %LPosList = role_data:get_lieuposList(),
                                            case lists:any(fun(E) ->
                                                                   #ger{gerBase=EBase} = E,
                                                                   EGerTypeID = EBase#gerBase.gerTypeID,
                                                                   #data_ger{sameType=ESameType} = data_ger:get(EGerTypeID),
                                                                   check_same_type(GerTypeID,EGerTypeID,SameType,ESameType) andalso EBase#gerBase.gerPos =/= GerPos 
                                                           end, LPosList) of
                                                true ->
                                                    {false, 4};
                                                false ->
                                                    PosList = role_data:get_posList(),
                                                    case lists:any(fun(E)->
                                                                           #ger{gerBase=EBase}=E,
                                                                           EGerTypeID = EBase#gerBase.gerTypeID, 
                                                                           #data_ger{sameType=ESameType} = data_ger:get(EGerTypeID),
                                                                           check_same_type(GerTypeID,EGerTypeID,SameType,ESameType)
                                                                   end, PosList) of
                                                        true ->
                                                            {false, 4};
                                                        false ->
                                                            {true, UpGer, LPosList, GerBag2}
                                                    end
                                            end;
                                        false ->
                                            {false, 2}
                                    end;
                                true ->
                                    {false, 6}
                            end
                    end
            end;
        X ->
            ?ERR("X:~w",[X]),
            {false, 5}
    end.


do_lieu_standup(GerPos, GerID, UpGer, PosList, GerBag2)->
    %%?ERR("装备小伙伴,位置:~p,ID:~p,Ger:~p,PosList:~p,GerBag2:~p.~n", [GerPos, GerID, UpGer, PosList, GerBag2]),
	GerTypeID = UpGer#gerSimple.gerTypeID,
	case util:fun_take(fun(E) ->GerPos == E#ger.gerBase#gerBase.gerPos end, PosList) of
		false ->
			GerTypeID2 = 0,
			%% 新的出站列表
			UpGer2 = ger_attr:recacl_lieu(UpGer#gerSimple{gerPos=GerPos}, PosList),
			PosList2 = [UpGer2|PosList],
			%% 重算天命
			PosList4 = [UpGer2|[destiny_impact_lieu(E,GerTypeID, GerTypeID2, PosList2)||E<-PosList]],
			GerBag3 = GerBag2,
            IsNeedCalc = true,
            ?CATCH(role_task_trigger:handle({dispach_task,role_up_lieu_ger_num,length(PosList4)}));
		{value, #ger{gerID=GerID2,gerBase=#gerBase{gerTypeID=GerTypeID2}}=DownGer, PosList2} ->
			%% 两个武将交换装备
			role_data:replace_equip(GerID,GerTypeID, GerID2),
			UpGer2 = ger_attr:recacl_lieu(UpGer#gerSimple{gerPos=GerPos}, PosList2),
			PosList3 = [UpGer2|PosList2],
			if GerTypeID2 == GerTypeID ->
                   IsNeedCalc = false,
				   PosList4 = PosList3;
			   true ->
				   %% 重算天命
				   PosList4 = [UpGer2 | [destiny_impact_lieu(E,GerTypeID, GerTypeID2, PosList3)||E<-PosList2]],
                   IsNeedCalc = true
			end,
			GerBag3 = [ger_lib:ger2gerSimple(DownGer)|GerBag2]
	end,
	role_data:set_gerBag(GerBag3),

	%ger_lib:notify_update(UpGer2),
	%role_data:init_lieuList(PosList4),
	%PosList5 = ger_attr:refresh_fightPower(PosList4),
	role_data:set_lieuposList(PosList4),
    case IsNeedCalc of
        true ->
            calc_destiny_num( );
        _ ->
            ignore
    end,
	?sendself(#sc_ger_lieu_standup{gerPos=GerPos, gerID=GerID, result=1}).

%%重算天命数
calc_destiny_num( ) ->
    GerList = role_data:get_posList( ) ++ role_data:get_lieuposList( ),
    {List, TypeIDList} = lists:foldl(fun(Ger, {IDAcc, TypeIDAcc}=Acc) ->
                                            case Ger of  
                                                #ger{gerID=ID, gerBase=#gerBase{gerTypeID=E}} ->
                                                    {[{ID, E}|IDAcc], [E|TypeIDAcc]};
                                                #gerSimple{gerID=ID, gerTypeID=E} ->
                                                    {[{ID, E}|IDAcc], [E|TypeIDAcc]};
                                                _ ->
                                                    Acc
                                            end
                                        end, {[],[]}, GerList),
  
    NumList = lists:foldl(fun({GerID, TypeID}, NumAcc) ->
						        EquipList = role_data:get_equip(GerID),
						        ArmedGerTypeIDList = TypeIDList -- [TypeID],
						        EquipTypeIDList = [ItemTypeID || #item{itemTypeID=ItemTypeID} <- EquipList],
						        #data_ger{destinyIDList=DesIDList} = data_ger:get(TypeID),
							    Num = lists:foldl(fun(DesID,C)->
												    #data_destiny{destinyType=DesType, destinyNeedList=DesNeedList} = data_destiny:get(DesID),
												    case ger_attr:check_destiny_add(DesType, ArmedGerTypeIDList, EquipTypeIDList, DesNeedList) of
													    true->
														    C+1;
													    false->
														    C
												    end
										          end, 0, DesIDList),
						        [Num|NumAcc]
                          end, [0], List),
    %%触发成就
    role_payGuide:trigger_task_change(?FORMATION_GER_DESTINY_TN,{0}),
    ?CATCH(role_task_trigger:handle({dispach_task,role_active_destiny_num,lists:max(NumList)})).

%% 计算小伙伴天命的函数。GerTypeID如果等于0，表示的是有小伙伴被卸载
destiny_impact_lieu(Ger,GerTypeID, GerTypeID2,PosList) ->
	#ger{gerBase=#gerBase{gerTypeID=T}} = Ger,
	%% 根据天命配置判断是否需要计算此武将属性
	case lists:any(fun(E) -> E=:=GerTypeID orelse E =:= GerTypeID2 end, data_destiny_rela:get(T)) of
		true ->
			Ger2 = ger_attr:recacl_lieu(Ger,PosList),
			ger_lib:notify_update(Ger2),
			Ger2;
		false ->
			Ger
	end.

% check dequeue
check_lieu_dequeue(GerPos, GerID)->
    LieuInfoT = role_data:get_lieutenantInfo(),
	GerBag = role_data:get_gerBag(),
    case lists:keyfind(GerPos, #t_lieu.pos, LieuInfoT) of
        #t_lieu{} ->
            LPosList = ger_attr:get_original_lieu_posList(),
			case lists:keytake(GerID, #ger.gerID, LPosList) of
				{value, UpGer, LPosList2} ->
					{true, UpGer, LPosList2, GerBag};
				_ ->
					{false, 3}
			end;
        X ->
            ?ERR("X:~w",[X]),
            {false, 5}
    end.

%% UpGer的类型是ger，这一点与do_lieu_standup不同
do_lieu_dequeue(GerPos, GerID, UpGer, LPosList2, GerBag)->
	GerTypeID = UpGer#ger.gerBase#gerBase.gerTypeID,
	%% 重算天命
%% ?DEBUG("L-do_lieu_dequeue-LPosList2 ~w",[LPosList2]),
	PosList3 = [destiny_impact_lieu(E,0, GerTypeID, LPosList2)||E<-LPosList2],
%% ?DEBUG("L-do_lieu_dequeue-PosList3 ~w",[PosList3]),
%% ?ERR("L-do_lieu_dequeue 11 Bag:~w lieuposList:~w",[[GerBag|UpGer],PosList3]),
	role_data:set_gerBag([ger_lib:ger2gerSimple(UpGer)|GerBag]),
	role_data:set_lieuposList(PosList3),   %% 此处会调用ger_attr:recacl_gers().重新计算主将，包括天命
%% ?ERR("L-do_lieu_dequeue 22 Bag:~w lieuposList:~w",[role_data:get_gerBag(),role_data:get_lieuposList()]),
	?sendself(#sc_ger_lieu_dequeue{gerPos=GerPos, gerID=GerID, result=1}).

do_ger_sell(GerBag2, _GerIDList, InfoAcc)->
	role_data:set_gerBag(GerBag2),
	LogGerList = [ [GerID,GerSimple#gerSimple.gerTypeID,GerSimple#gerSimple.gerLevel,GerSimple#gerSimple.gerQuality]  || {_,#gerSimple{gerID=GerID}=GerSimple}<- InfoAcc],
	#role{roleID=RoleID} = RoleInfo = role_data:get_roleInfo(),
	{Date, _} = Time = erlang:localtime(),
  [erlang:send(self(), {ger_del_cancel_cirrus, GerID})||[GerID,_,_,_]<-LogGerList],
	behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_SELL_GER, 0, ""),
	{AddCoin, ItemList} = calc_split(InfoAcc),
	role_lib:add_coin_f(RoleInfo, AddCoin, ?MONEY_ADD_TYPE_SELL_GER, 0, ""),
	item_lib:add_item_f(ItemList, ?MONEY_ADD_TYPE_SELL_GER, 0, ""),
  %%推送删除精灵协议
  ger_lib:ra_ger_delete([GerSimple||{_,GerSimple}<-InfoAcc]),
	P_reward_list = [#p_reward_view3{type=1,typeID=TypeID,num=Num}||#new_item{itemTypeID=TypeID, itemNum=Num}<-ItemList],
	?sendself(#sc_ger_sell{result=1, reward=[#p_reward_view3{type=1, typeID=20007, num=AddCoin}]++P_reward_list}).

calc_split(InfoAcc)->
	lists:foldl(fun({#data_ger{price=SplitID}, #gerSimple{gerQuality = GerRank,gerLevel=GerLevel}}, {CoinAcc, ItemAcc})->
						#data_card_split{coin=Coin, itemList=ItemList} = data_card_split:get(SplitID),
						{CoinT, NewItemList} = role_item:get_item_split(Coin, ItemList, GerRank+1, ItemAcc,GerRank, GerLevel),
						{CoinAcc + CoinT, NewItemList}
						end, {0, []}, InfoAcc).

check_sell(GerIDList) ->
	GerBag = role_data:get_gerBag(),
	util:foldl(fun(GerID,{GerBagAcc, DelAcc}) ->
						case check_sell2(GerID,GerBagAcc) of
							{false, _Reason} =R->
								{return, R};
							{true, DataGer, GerInfo, GerBagAcc2} ->
								{GerBagAcc2, [{DataGer,GerInfo}|DelAcc]}
						end
			   end, {GerBag,[]}, GerIDList).

check_sell2(GerID, GerBag) ->
	case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
		{value, GerInfo, GerBag2} when is_record(GerInfo, gerSimple) ->
			#gerSimple{gerTypeID=GerTypeID} = GerInfo,
					case data_ger:get(GerTypeID) of
            %%v4.0版本开启精灵出售功能，修改price来限制能够出售的精灵ID
						#data_ger{price=-1}->
              {false,3};
            DataGer when is_record(DataGer, data_ger) ->
							{true, DataGer, GerInfo, GerBag2};
						_ ->
							{false, 4}
					end;
		false ->
			{false, 2}
	end.

check_move_pos(GerPos, TargetPos) ->
	if TargetPos < 1 orelse TargetPos > 6 ->
		   {false,2};
	   true ->
		   PosList = ger_attr:get_original_posList(),
		   %PosList = role_data:get_posList(),
		   case util:fun_take(fun(E) -> GerPos== E#ger.gerBase#gerBase.gerPos end, PosList) of
			   false ->
				   {false, 3};
			   {value, E, PosList2} ->
				   case util:fun_take(fun(E1) -> TargetPos== E1#ger.gerBase#gerBase.gerPos end, PosList2) of
					   false ->
                           {true, [?change_pos(E,TargetPos)],PosList2};
					   {value, E2, PosList3} ->
                           {true, [?change_pos(E2,GerPos), ?change_pos(E,TargetPos)],PosList3}
				   end
		   end
	end.
					
check_standup(GerPos, GerID) ->
	GerBag = role_data:get_gerBag(),
	PosList = ger_attr:get_original_posList(),
	#role{level=Level} = role_data:get_roleInfo(),
	GerStandLimit = data_stand_fighters:get(Level),
	IsReplace = lists:any(fun(#ger{gerBase=#gerBase{gerPos=EGerPos}}) -> EGerPos == GerPos end, PosList),
	case length(PosList) >= GerStandLimit andalso not(IsReplace) of
		true ->
			{false,7};
		_ ->
			case role_ger:is_mirror_ger(GerID) of
				true ->
					{false, 6};
				false ->
					case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
						false ->
							{false, 3};
						{value, #gerSimple{gerTypeID=GerTypeID}=UpGer, GerBag2} ->
							case util:is_exp_card(GerTypeID) of
								false ->
									case (is_integer(GerPos) andalso GerPos >0 andalso GerPos =< 6) of
										true ->
                                            #data_ger{sameType=SameType} = data_ger:get(GerTypeID),
											%PosList = role_data:get_posList(),
											case lists:any(fun(E) ->
																   #ger{gerBase=EBase} = E,
                                                                   EGerTypeID = EBase#gerBase.gerTypeID, 
                                                                    #data_ger{sameType=ESameType} = data_ger:get(EGerTypeID),
																   check_same_type(GerTypeID,EGerTypeID,SameType,ESameType) andalso EBase#gerBase.gerPos =/= GerPos 
														   end, PosList) of
												true ->
													{false, 4};
												false ->
                                                    %%v4.1版本将小伙伴从上阵精灵的概念中独立出来了，此处不需要再判断
													{true, UpGer, PosList, GerBag2}
											end;
										false ->
											{false, 2}
									end;
								true ->
									{false, 5}
							end
					end
			end
	end.

%check_same_type(GerTypeID,EGerTypeID,GerTypeID,EGerTypeID) -> GerTypeID == EGerTypeID;
check_same_type(_GerTypeID,EGerTypeID,SameType,EGerTypeID) -> EGerTypeID == SameType;
check_same_type(GerTypeID,_EGerTypeID,GerTypeID,ESameType) -> GerTypeID == ESameType;
check_same_type(GerTypeID,EGerTypeID,_,_) -> GerTypeID == EGerTypeID.

check_same_type2(GerTypeID,GerTypeID)->
    true;
check_same_type2(GerTypeIDA,GerTypeIDB)->
    #data_ger{sameType=SameTypeA} = data_ger:get(GerTypeIDA),
    #data_ger{sameType=SameTypeB} = data_ger:get(GerTypeIDB),
    SameTypeA=:=GerTypeIDB orelse SameTypeB=:=GerTypeIDA orelse SameTypeA=:=SameTypeB.

check_unload(UnLoadPos) ->
    case UnLoadPos > 0 andalso UnLoadPos =< 6 of
        true ->
            PosList = role_data:get_posList(),
            case erlang:length(PosList) =:= 1 of
                true ->
                    {false, 4};
                _ ->
                    case util:fun_take(fun(#ger{gerBase=#gerBase{gerPos=GerPos}}) -> 
                                            UnLoadPos =:= GerPos end, PosList) of
                        false ->
                            {false,3};
                        {value, UnLoadGer, PosList2} ->
                            {true, UnLoadGer, PosList2}
                end
            end;
        _ ->
            {false, 2}
    end.

	
check_order(PosList) ->
	Role = role_data:get_roleInfo(),
	MaxNum = data_ger_num:get(Role#role.level),
	%% 最大出战个数
	if length(PosList) > MaxNum ->
		   {false, 3};
	   true ->
		   {Pos,IDList} = lists:foldl(fun(#posInfo{gerID=GerID,gerPos=GerPos}, {PosAcc,IDAcc}) ->
											  {[GerPos|PosAcc],[GerID|IDAcc]}
									  end, {[], []}, PosList),
		   %% 是否有重复项
		   case ((util:is_duplicate(Pos)=:=false) andalso (util:is_duplicate(IDList)=:=false)) of
			   true ->
				   %% 是否拥有该武将
				   case lists:all(fun(E) ->role_lib:has_gerID(E) end, IDList) of
					   true ->
						   %% 位置参数是否错误
						   case lists:all(fun(E) -> is_integer(E) andalso E>=0 andalso E=<6 end, Pos) of
							   true ->
								   true;
							   false ->
								   {false, 2}
						   end;
					   false ->
						   {false, 4}
				   end;
			   false ->
				   {false, 2}
		   end
	end.
						
	
%% 先从随机列表删除已经存在的属性id
get_init_lieutenant(Pos,{SpecialList,AttAddID,HpAddID})->
	SpecialList2 = lists:foldl(fun(E,Acc)->lists:delete(E, Acc) end, SpecialList, get_lieu_spID_list()),
	SpecialID = util:random_one_from_list(SpecialList2),
	{t_lieu,Pos,SpecialID,0,AttAddID,0,HpAddID,0}.

get_lieu_spID_list()->
	lists:foldl(fun(#t_lieu{infoID1=InfoID1}, Acc)->
						[InfoID1|Acc] end, [], role_data:get_lieutenantInfo()).

is_mirror_ger(GerUid)->
    #gerMirrorInfo{gerMirrorState={GerSimpleInfo,_}}  = role_data:get_mirror(),
    GerUid =:= GerSimpleInfo#gerSimple.gerID.    
	
%% ====================================================================
%% Internal functions
%% ====================================================================


take_ger(GerID, PosList, GerBag) ->
	case lists:keytake(GerID, #ger.gerID, PosList) of
		false ->
			case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
				false ->
					false;
				{value, Ger, GerBag2} ->
					{value, Ger, PosList, GerBag2}
			end;
		{value, Ger, PosList2} ->
			{value, Ger, PosList2, GerBag}
	end.

take_ger2(GerID) ->
	case lists:keyfind(GerID, #ger.gerID, role_data:get_posListT()) of
		#ger{}=Ger ->
			Ger;
		_ ->
			#ger{}
	end.
		

set_ger(Ger, PosList, GerBag) ->
	if is_record(Ger, gerSimple) ->
			{PosList, [Ger|GerBag]};
		true ->
			{[Ger|PosList], GerBag}
	end.

take_ger(GerID, PosList, LPosList, GerBag) ->
	case lists:keytake(GerID, #ger.gerID, PosList) of
		false ->
			case lists:keytake(GerID, #ger.gerID, LPosList) of
				false ->
					case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
						false ->
							false;
						{value, Ger, GerBag2}->
							{value, Ger, PosList, LPosList, GerBag2, bag}
					end;
				{value, Ger, LPosList2}->
					{value, Ger, PosList, LPosList2, GerBag, lieu}
			end;
		{value, Ger, PosList2}->
			{value, Ger, PosList2, LPosList, GerBag, ger}
	end.

set_ger(Ger, PosList, LPosList, GerBag, Type) ->
	case Type of
		bag ->
			{PosList, LPosList, [Ger|GerBag]};
		lieu ->
			{PosList, [Ger|LPosList], GerBag};
		ger ->
			{[Ger|PosList], LPosList, GerBag}
	end.

test(RoleID)->
	role_ger:cs_ger_view_other_dtl(#cs_ger_view_other_dtl{tarRoleID = RoleID}),
	role_ger:cs_ger_view_other(#cs_ger_view_other{tarRoleID=RoleID}).

%%根据精灵typeID返回对应的精灵蛋ID,21为2星精灵蛋.....
get_ger_egg_typeid(GerTypeID)->
	case data_ger:get(GerTypeID) of
		?undefined->
			21;
		#data_ger{gerStar=Star,sameType=SameType}->
			ID = case SameType of GerTypeID -> data_ger_up_rank:get({data_ger_star,Star});
                     _-> data_ger_up_rank_light:get({data_ger_star,Star})
                 end,
            case ID of 
				?undefined->
					21;
				ID ->
					ID
			end
	end.

%%返回精灵退化之后应该返回的精灵蛋个数
get_down_rank_return_egg_num(GerTypeID,GerQuality)->
  case GerQuality >= ?MAX_GER_RANK of
    false->
      1;
    true->
      NewGerQuality = case GerQuality of
        24->
          23;
        29->
          28;
        _->
          GerQuality
      end,
      #data_ger{gerSecondEvolutionID=GerSecondEvolutionID} = data_ger:get(GerTypeID),
      {_CoinCost,GerNum} = data_second_evolution:get({GerSecondEvolutionID,NewGerQuality}),
      GerNum
  end.

%%添加用于区分是否是黑白龙类型的
is_specail_ger(GerTypeID)->
    lists:member(GerTypeID,data_ger_up_rank:get(special_ger_list)).

cs_ger_transform_blink(#cs_ger_transform_blink{devourID=DevourID,devouredID=DevouredID})->
  case check_devoured_ger(DevouredID) of
    {false,R}->
      ?sendself(#sc_ger_transform_blink{result=R,newblinkger=#p_ger{}});
    {DevouredGer,GerBagR,DevouredGerTypeID,DevouredGerType}->
      case check_devour_ger(DevourID) of
        {false,R}->
          ?sendself(#sc_ger_transform_blink{result=R,newblinkger=#p_ger{}});
        {DevourGer,PosListR,DevourGerTypeID,DevourGerType}->
          case check_ger_type_blink(DevourGerTypeID,DevouredGerTypeID,DevourGerType=/=DevouredGerType) of
            {false,R}->
              ?sendself(#sc_ger_transform_blink{result=R,newblinkger=#p_ger{}});
            {true,TransformType}->
              %%转换之前卸载下所有的技能宝石
              #role{roleID=RoleID} = role_data:get_roleInfo(),
              DevourGer1 = role_diamond:demount_all_diamond_for_ger(DevourGer),
              NewDevourGer = do_transform_blink(DevourGer1,DevouredGer,RoleID,TransformType),
              SrcGer = role_crystal:refresh_gercrystal(NewDevourGer),
              SrcGer1 = ger_attr:recacl(SrcGer, PosListR),
              %%此处将sc_ger_transform_blink协议提前，这个协议中的战斗部分只是posList里面的属性，不是最终的面板数据，需要后面的set_posList中的notify来更新面板协议，故要提前到set_posList前
              %%但是sc_ger_update里面没有包含觉醒等信息，故利用sc_ger_transform_blink推送觉醒
              ?sendself(#sc_ger_transform_blink{result=1,newblinkger=ger_lib:ger2p_ger_view_dtl(SrcGer1)}),
              PosList1 = [SrcGer1|PosListR],
              %%更新上阵精灵队形
              role_data:set_posList(PosList1),
              role_data:set_gerBag(GerBagR),
              % ger_lib:notify_update(SrcGer1),
              ger_lib:ra_ger_delete([DevouredGer])
          end
      end
  end.

%%检查被吞噬精灵是否符合条件
check_devoured_ger(DevouredID)->
  case is_mirror_ger(DevouredID) of
    false->
      #role{roleID=RoleID} = role_data:get_roleInfo(),
      case homestead_server:has_homestead_ger(RoleID,[DevouredID]) of
        false->
          BagGer = role_data:get_gerBag(),
          case lists:keytake(DevouredID,#gerSimple.gerID,BagGer) of
            false->
              {false,5};
            {_Vaule,DevouredGer=#gerSimple{gerTypeID=GerTypeID},Other}->
                  {DevouredGer,Other,GerTypeID,ger_lib:is_blink_ger(GerTypeID)}
          end;
        true->
          {false,2}
      end;
    true->
      {false,4}
  end.

%%检查吞噬精灵是否符合条件
check_devour_ger(DevourID)->
  PosList = role_data:get_posList(),
  case lists:keytake(DevourID,#ger.gerID,PosList) of
    {_Vaule,#ger{gerBase=#gerBase{gerTypeID=GerTypeID}}=DevourGer,Other}->
          {DevourGer,Other,GerTypeID,ger_lib:is_blink_ger(GerTypeID)};
    false->
      {false,7}
  end.

%%检查吞噬者和被吞噬者是否处于同一种类型精灵
check_ger_type_blink(_DevourGerTypeID,_DevouredGerTypeID,false)->
  {false,6};
check_ger_type_blink(DevourGerTypeID,DevouredGerTypeID,true)->
  case data_ger:get(DevourGerTypeID) of
    #data_ger{sameType=DevouredGerTypeID}->
      {true,blink_eat_normal};
    _->
      case data_ger:get(DevouredGerTypeID) of
        #data_ger{sameType=DevourGerTypeID}->
          {true,normal_eat_blink};
        _->
          {false,9}
      end
  end.

%%完成闪光精灵对非闪光精灵的继承
do_transform_blink(DevourGer,DevouredGer,RoleID,TransformType)->
  #gerSimple{gerQuality=GerQuality1,gerLevel=GerLevel1,gerExp=GerExp1,
    gerAwakeInfo=GerAwakeInfo1,gerCrystalInfo=GerCrystalInfo1,gerHolyGrailInfo=GerHolyGrailInfo1,gerTypeID=GerTypeID1} = DevouredGer,
  #ger{gerBase=GerBase,gerID=GerID2} = DevourGer,
  #gerBase{gerQuality=GerQuality2,gerLevel=GerLevel2,gerExp=GerExp2,gerTypeID=GerTypeID2,
    gerAwakeInfo=GerAwakeInfo2,gerCrystalInfo=GerCrystalInfo2,gerHolyGrailInfo=GerHolyGrailInfo2} = GerBase,
  {NewGerExp,NewGerLevel,NewGerQuality} = transform_exp_and_level_and_quality(GerExp1,GerLevel1,GerExp2,GerLevel2,GerQuality1,GerQuality2,GerID2,GerTypeID2,RoleID),
  NewAwakeInfo = transform_awakeinfo(GerAwakeInfo1,GerAwakeInfo2,GerID2,GerTypeID2,RoleID),
  NewCrystalInfo = transform_crystalinfo(GerCrystalInfo1,GerCrystalInfo2,GerID2,GerTypeID2,RoleID),
  NewHolyGrailInfo = transform_holyGrailinfo(GerHolyGrailInfo1,GerHolyGrailInfo2),
  NewGerBase = GerBase#gerBase{gerQuality=NewGerQuality,gerLevel=NewGerLevel,gerExp=NewGerExp,gerAwakeInfo=NewAwakeInfo,
    gerCrystalInfo=NewCrystalInfo,gerHolyGrailInfo=NewHolyGrailInfo},
  case TransformType of
    normal_eat_blink->
      role_gather:hook_add_ger_list([{GerTypeID1,NewGerQuality}]),
      DevourGer#ger{gerBase=NewGerBase#gerBase{gerTypeID=GerTypeID1}};
    blink_eat_normal->
      role_gather:hook_add_ger_list([{GerTypeID2,NewGerQuality}]),
      DevourGer#ger{gerBase=NewGerBase#gerBase{gerTypeID=GerTypeID2}}
  end.

%%完成对经验、等级和品阶的转换
transform_exp_and_level_and_quality(GerExp1,GerLevel1,GerExp2,GerLevel2,GerQuality1,GerQuality2,GerID2,GerTypeID2,RoleID)->
  {Date, _} = Time = erlang:localtime(),
  {NewExp,NewLevel} = case GerExp2 > GerExp1 of
    false->
      {GerExp1,GerLevel1};
    true->
      NewGerLevel = data_ger_exp_light:get(GerExp2),
      behavior_ger_uplevel:log(RoleID,GerID2,GerTypeID2,GerLevel2,GerExp2,GerQuality2,NewGerLevel,GerExp1,Date,Time),
      {GerExp2,NewGerLevel} 
  end,
  NewGerQuality = case GerQuality2 > GerQuality1 of
    false->
      GerQuality1;
    true->
      behavior_ger_uprank:log(RoleID,GerID2,GerTypeID2,GerLevel1,GerExp1,GerQuality1,NewLevel,NewExp,GerQuality2,0, Date,Time),
      GerQuality2
  end,
  {NewExp,NewLevel,NewGerQuality}.

transform_awakeinfo(GerAwakeInfo1,GerAwakeInfo2,GerID,GerTypeID,RoleID)->
  lists:foldl(fun(Step,Acc)->
    StepAwake1 = role_awake:get_step_awake(Step,GerAwakeInfo1),
    StepAwake2 = role_awake:get_step_awake(Step,GerAwakeInfo2),
    Time = erlang:localtime(),
    case transform_single_step_awake(StepAwake1,StepAwake2,GerID,GerTypeID,RoleID,Time) of
      ?undefined->
        Acc;
      N->
        [N|Acc]
    end
  end,[],data_awake:get(mega_step_list)).

%%根据觉醒技能当前品阶确定是否继承，只继承了当前的技能ID
transform_single_step_awake([],[],_GerID,_GerTypeID,_RoleID,_Time)->
  ?undefined;
transform_single_step_awake([],GerAwake2=#awake{step=AwakeStep,skillID=SkillID},GerID,GerTypeID,RoleID,{Date,_}=Time)->
  behavior_ger_awake:log(RoleID,GerID,GerTypeID,AwakeStep,0,SkillID,[],[],0,0,Date,Time,6),
  GerAwake2#awake{recast_time=0,new_skilllist=[]};
transform_single_step_awake(GerAwake1,[],_GerID,_GerTypeID,_RoleID,_Time)->
  GerAwake1;
transform_single_step_awake(GerAwake1,GerAwake2,GerID,GerTypeID,RoleID,{Date,_}=Time)->
  #awake{skillID=SkillID1,skill_quality=SkillQuality1,step=AwakeStep,new_skilllist=SkillList1,recast_time=RecastTime1} = GerAwake1,
  #awake{skillID=SkillID2,skill_quality=SkillQuality2} = GerAwake2,
  case SkillQuality1 > SkillQuality2 of
    false->
      {Date, _} = Time = erlang:localtime(),
      SkillListLog = role_awake:transform_skillid2log(SkillList1),
      behavior_ger_awake:log(RoleID,GerID,GerTypeID,AwakeStep,SkillID1,SkillID2,SkillListLog,SkillListLog,RecastTime1,RecastTime1,Date,Time,6),
      GerAwake1#awake{skillID=SkillID2,skill_quality=SkillQuality2};
    true->
      GerAwake1
  end.

%%转换晶体
transform_crystalinfo(GerCrystalInfo1,GerCrystalInfo2,GerID,GerTypeID,RoleID)->
  lists:foldl(fun(CrystalType,Acc)->
    Crystal1 = role_crystal:get_crystal_by_type(CrystalType,GerCrystalInfo1),
    Crystal2 = role_crystal:get_crystal_by_type(CrystalType,GerCrystalInfo2),
    Time = erlang:localtime(),
    case transform_single_crystal(Crystal1,Crystal2,GerID,GerTypeID,RoleID,Time) of
      ?undefined->
        Acc;
      N ->
        [N|Acc]
    end
  end,[],data_crystal:get(data_crystal_type)).

transform_single_crystal(?undefined,?undefined,_GerID,_GerTypeID,_RoleID,_Time)->
  ?undefined;
transform_single_crystal(?undefined,Crystal2,GerID,GerTypeID,RoleID,{Date,_}=Time)->
  #crystal{crystaltype=CrystalType,crystalquality=CrystalQuality,crystallevel=CrystalLevel,
    crystalexp=CrystalExp,crystalrankexp=CrystalRankExp} = Crystal2,
  behavior_ger_crystal:log(RoleID,GerID,GerTypeID,CrystalType,0,CrystalQuality,0,CrystalLevel,0,CrystalExp,0,CrystalRankExp,
      Date,Time,5),
  Crystal2;
transform_single_crystal(Crystal1,?undefined,_GerID,_GerTypeID,_RoleID,_Time)->
  Crystal1;
transform_single_crystal(Crystal1,Crystal2,GerID,GerTypeID,RoleID,{Date,_}=Time)->
  #crystal{crystaltype=CrystalType1,crystalquality=CrystalQuality1,crystallevel=CrystalLevel1,
    crystalexp=CrystalExp1,crystalrankexp=CrystalRankExp1} = Crystal1,
  #crystal{crystalquality=CrystalQuality2,crystallevel=CrystalLevel2,
    crystalexp=CrystalExp2,crystalrankexp=CrystalRankExp2} = Crystal2,
  case (CrystalQuality2 > CrystalQuality1) orelse 
       ((CrystalQuality2 =:= CrystalQuality1) andalso (CrystalLevel2>CrystalLevel1)) orelse
       ((CrystalQuality2 =:= CrystalQuality1) andalso (CrystalLevel2 =:=CrystalLevel1) andalso (CrystalRankExp2>CrystalRankExp1)) orelse
       ((CrystalQuality2 =:= CrystalQuality1) andalso (CrystalLevel2 =:=CrystalLevel1) andalso (CrystalRankExp2 =:= CrystalRankExp1) andalso (CrystalExp2>CrystalExp1)) of
      true->
        behavior_ger_crystal:log(RoleID,GerID,GerTypeID,CrystalType1,CrystalQuality1,CrystalQuality2,CrystalLevel1,
          CrystalLevel2,CrystalExp1,CrystalExp2,CrystalRankExp1,CrystalRankExp2,Date,Time,5),
        Crystal2;
      false->
        Crystal1
  end.

%%转换神器
transform_holyGrailinfo(GerHolyGrailInfo1,GerHolyGrailInfo2)->
  #holyGrail{holyGrailLevel=HolyGrailLevel1,diamondInfo=DiamondInfo} = GerHolyGrailInfo1,
  #holyGrail{holyGrailLevel=HolyGrailLevel2,isFinishSacrifice=IsFinishSacrifice2} = GerHolyGrailInfo2,
  case ((HolyGrailLevel2 > HolyGrailLevel1) andalso(DiamondInfo=:=[])) orelse
       ((HolyGrailLevel2 =:= HolyGrailLevel1) andalso(DiamondInfo=:=[]) andalso (IsFinishSacrifice2=:=true)) of
    true->
      GerHolyGrailInfo2;
    false->
      GerHolyGrailInfo1
  end.