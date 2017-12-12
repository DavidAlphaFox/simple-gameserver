%% @author caohongyang
%% @doc 玩家数据持久化接口
%% Created 2013-3-11

%% 等待优化点：只遍历一次进程字典，完成玩家数据持久化。

-module(role_persist).
-include("def_role.hrl").
-include("def_battle.hrl").
-compile(export_all).
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 持久化所有数据到db
persist_all_data() ->
	RoleID = role_data:get_roleID(),
	%% 先做一次重要数据的定时持久化
	do_interval_persist(RoleID),
	do_interval_persist2(RoleID),
	%% 再做不重要数据的持久化
	do_less_important_data_persist(RoleID).

do_less_important_data_persist(RoleID) ->	
	%persist_battle(RoleID),
	persist_role_task(RoleID),
	persist_sign_emperor_info(RoleID),
	persist_box_free(RoleID),
    persist_whisper_list(RoleID),
	persist_role_monthVIP(RoleID),
	role_discount:persist_role_discount_info(RoleID),
	role_panicbuy:persist_role_panicbuy_info(RoleID),
	persist_role_treaHouseInfo(RoleID),
	persist_role_mirror(RoleID),
	persist_lucky_roll(RoleID),
	persist_vip_shop(RoleID),
	persist_tvcard(RoleID),
	persist_bounty(RoleID),
	persist_plane_info(RoleID),
	persist_homeBoss_times(RoleID),
	persist_shopBoxData(RoleID),
	persist_payGuide(RoleID),
	persist_headSeven(RoleID),
	persist_activityFestival(RoleID),
	persist_exBoss(RoleID),
    persist_dSignData(RoleID),
    	persist_xbattle(RoleID),
    	persist_manual(RoleID),
    	persist_tag_data(RoleID),
	persist_trainerRear(RoleID),
    	persist_trainerProf_battle_data(RoleID),
	lists:foreach(fun({Key,Value}) ->persist_other(RoleID, Key,Value) end, erlang:get()),
	ok.

do_interval_persist(RoleID) ->
	persist_roleInfo(),
	persist_roleExtra(RoleID),
	persist_trSpecial(RoleID),
	persist_roleBag(RoleID),
    persist_role_skin(RoleID),
	persist_playerTrainingRoom(RoleID),
	ok.

do_interval_persist2(RoleID) ->
	persist_ger_list(RoleID),
	persist_gag_list(RoleID),
	persist_fighter_list(RoleID),
	persist_role_talent(RoleID),
    db_sql:set_role_tasklink(RoleID,role_tasklink:get_role_tasklink()),
    db_sql:set_growth_fund_record(RoleID,role_data:get_growth_fund_record()),
    db_sql:set_dojang_data(RoleID,role_data:get_dojang_data()),
    persist_role_canvass_info(RoleID),
    db_sql:set_role_dojangrank(RoleID,role_data:get_dojangrank_data()),	persist_role_lieu(RoleID),
	persist_role_encounterInfo(RoleID),	
	ok.
	
%%persist_battle(RoleID) ->
%%	ChapterList = [Chapter|| {{?chapter, _}, Chapter} <- erlang:get()],
%%	if ChapterList =/= [] ->
%%	{ok,_} = db_sql:set_chapter(RoleID, ChapterList);
%%	   true ->
%%		   ignore
%%	end.
%%
persist_tag_data(RoleID)->
    db_sql:set_role_tag_data(RoleID, role_data:get_tag_data()).
persist_activityFestival(RoleID) ->
	db_sql:set_role_activityFestival(RoleID,role_data:get_activityFestival()).

persist_dSignData(RoleID) ->
    db_sql:set_dSignData(RoleID,role_data:get_dSignData()).

persist_payGuide(RoleID) ->
	db_sql:set_payGuide(RoleID,role_data:get_payGuide()).

persist_headSeven(RoleID)->
	db_sql:set_role_headSeven(RoleID,role_data:get_headSeven()).

persist_manual(RoleID)->
	db_sql:set_manualInfo(RoleID,role_gather:get_role_manual()).

persist_trainerRear(RoleID)->
	role_trainerRear:persist_trainerRear(RoleID).

persist_trainerProf_battle_data(RoleID) ->
    db_sql:set_trainerProf_battle_data(RoleID,role_data:get_trainerProf_battle_data()).	
persist_shopBoxData(RoleID) ->
	db_sql:set_role_box_shop(RoleID,role_data:get_shopBoxData()).
	
persist_trSpecial(RoleID) ->
	db_sql:set_trSpecial_info(RoleID,role_data:get_trSpecial()).

persist_exBoss(RoleID)->
	db_sql:set_exBoss_data(RoleID,role_data:get_exBoss_data()).
persist_xbattle(RoleID)->
	[db_sql:set_xbattle_chapter(RoleID, Chapter)
     || {{?chapter_x, _}, Chapter} <- erlang:get()],
    db_sql:set_xbattle_data(RoleID,role_data:get_xbattle_data()),
    db_sql:set_xbattle_ger_display(RoleID,role_xbattle:get_ger_display()).

persist_homeBoss_times(RoleID)->
	db_sql:set_role_homeBoss_times(RoleID,role_data:get_homeBoss_times()).

persist_plane_info(RoleID) ->
	db_sql:set_carlos_plane_info(RoleID,role_data:get_plane_use_info()).

persist_tvcard(RoleID) ->
	db_sql:set_tvcard(RoleID,role_data:get_tvcard()).

persist_bounty(RoleID) ->
    F = lists:foldl(fun(Type,AccList)-> 
                        ChapterList = data_bounty:get({type,Type}),
                        lists:foldl(fun(ChapterID,SubAccList)-> 
                                            [{role_data:get_bounty_fighters(ChapterID),ChapterID}|SubAccList] 
                                    end, AccList, ChapterList)
                end, [], [1,2,3]),
	db_sql:set_bounty_info(RoleID,role_data:get_bountyInfo(),F).

persist_role_monthVIP(RoleID) ->
	db_sql:set_role_monthVIP_info(RoleID,role_data:get_role_monthVIP_info()).

persist_whisper_list(TarRoleID) ->
    case get(?whisper_list) of
        WhisperList when is_list(WhisperList)->
            lists:foreach(fun({p_whisper_record,SendRoleID,TalkMessage,TimeStamp})->
                                  db_sql:add_whisper(TarRoleID,SendRoleID,TalkMessage,TimeStamp)
                        end, WhisperList);
        _ ->
            ingore
    end.

persist_box_free(RoleID) ->
    db_sql:set_role_free_box_info(RoleID, role_data:get_free_box_info()).

persist_lucky_roll(RoleID) ->
	db_sql:set_lucky_roll(RoleID,role_data:get_lucky_roll()).

persist_vip_shop(RoleID) ->
	db_sql:set_vip_shop(RoleID,role_data:get_vip_shop()).
 
persist_gag_list(RoleID)->
	GagList = role_data:get_gag_list(),
	db_sql:set_role_gag_list(RoleID, GagList).

persist_sign_emperor_info(RoleID)->
	Info = role_data:get_sign_emperor_info(),
	db_sql:set_sign_emperor_info(RoleID, Info).

persist_role_lieu(RoleID)->
	LieuInfo=role_data:get_lieutenantInfo(),
	db_sql:set_role_lieutenant_info(RoleID, LieuInfo).

persist_role_canvass_info(RoleID)->
    case erlang:get(?canvass_info) of
        ?undefined-> ignore;
        {CanvassId,CanvassInfo} -> 
            db_sql:set_canvass_info(RoleID,CanvassId,CanvassInfo)
    end.

persist_role_treaHouseInfo(RoleID)->
	TreaHouseInfo = role_data:get_treaHouseInfo(),
	db_sql:set_treasure_house_info(RoleID, TreaHouseInfo). 

persist_role_task(RoleID)->
	TaskList = role_data:get_task_list(),
	db_sql:set_task(RoleID,TaskList).

persist_role_talent(RoleID)->
    TalentList = role_data:get_trainer_info(),
    db_sql:set_trainer_info(RoleID,TalentList).

persist_role_mirror(RoleID)->
    case erlang:get(?roleGerMirror) of
        Info when erlang:is_record(Info, gerMirrorInfo) ->
            db_sql:set_gerMirrorInfo(RoleID,Info);
        ?undefined ->
            ignore;%没用过这个数据不必保存什么了
        Info ->
            ?ERR("魔精功能数据保存时，数据格式异常(~w)~w",[RoleID,Info])
    end.

%% 持久化其他数据
persist_other(RoleID, ?guideVal, GuideVal) ->
    db_sql:log_guide(RoleID, GuideVal);
persist_other(RoleID, ?teamPkInfo, TeamPkInfo) ->
    db_sql:set_team_pk_data(RoleID, TeamPkInfo);
persist_other(RoleID, ?alienInfo, AlienInfo) ->
    db_sql:set_alien_data(RoleID, AlienInfo);
persist_other(_,_,_) ->
    ok.

persist_roleInfo() ->
	NowSec = util:now(),
	RoleInfo = role_data:get_roleInfo(),
	FightPower = role_data:get_roleFightPower(),
	RoleInfo2 = RoleInfo#role{fightPower=FightPower,lastLogoutTime=NowSec},
	db_sql:update_roleInfo(RoleInfo2),
	role_lib:update_rolePublic(RoleInfo2,role_data:get_trSpecial(),role_data:get_plane_use_info(),role_data:get_xbattle_data()).

persist_roleExtra(RoleID) ->
	RoleExtra = role_data:get_roleExtra(RoleID),
	persist_roleExtra2(RoleID,RoleExtra).

persist_roleExtra2(RoleID, RoleExtra) ->	
	#roleExtra{battleProgress=BattleProgress,
			   battleProgressHard=BattleProgressHard,
			   battleProgressFastHard=BattleProgressFastHard,
               battleProgressTransmigration=BattleProgressTransmigration,
			   cardInfo=CardInfo,
			   dailyInfo=DailyInfo,
			   encounterList=EncounterList,
			   gatherList=_GatherList,
			   hronInfo=HronInfo,
			   limitInfo=LimitInfo,
			   roleTimes=RoleTimes,
			   shopNumList=ShopNumList,
			   randomShopList=RandomShopList,
               itemUseList=ItemUseList} = RoleExtra,
	?DEBUG("set randomShopList=~w",[RandomShopList]),
    MagicBookStateStr = role_data:get_magicBook_state_str(),
    SignInfo = role_data:get_signinfo(),
    BattleBossRewardInfo =role_data:get_battlebossrewardinfo(),
    HronHistory = role_data:get_hronhistory(),
    MainGerTypeID = role_data:get_main_gerTypeID(),
    VTrainerProf = role_trainerProf:get_all_trainerProf_Persist(),
    SignSec = role_data:get_signSec(),
	db_sql:set_roleExtra(RoleID, BattleProgress,BattleProgressHard,BattleProgressFastHard,BattleProgressTransmigration
                         ,RoleTimes, EncounterList, DailyInfo, RandomShopList, ItemUseList, MagicBookStateStr
                        ,SignInfo,BattleBossRewardInfo,MainGerTypeID,VTrainerProf,SignSec),
	if is_record(CardInfo, cardInfo) ->
		   db_sql:set_cardInfo(RoleID, CardInfo);
	   true ->
		   ignore
	end,
	db_sql:set_hronInfo(RoleID, HronInfo,HronHistory),
	db_sql:set_limitInfo(RoleID, LimitInfo),
    db_sql:set_shopNumList(RoleID, ShopNumList).

persist_role_encounterInfo(RoleID)->
	EncounterInfo = role_data:get_roleEncounterInfo(),
	db_sql:set_role_encounterList(RoleID, EncounterInfo).

persist_ger_list(RoleID) ->
	GerList = role_data:get_gerBag(),
	PosList = role_data:get_posList(),
	LPosList  = role_data:get_lieuposList(),
	db_sql:set_gerList(RoleID, PosList, GerList,LPosList).
	
persist_fighter_list(RoleID) ->
	FighterList = role_data:get_fighter_list(),
	?DEBUG("fight:~w",[FighterList]),
	LieuInfoList = role_data:get_lieuInfoList(),
	LieuAddAttr = role_data:get_lieu_add_attr(),
	{AtkAdd,HpAdd} = role_buddy:get_buddy_normal_buff_for_total(),
	TrSpecial = role_data:get_trSpecial(),
	db_sql:set_fighterList(RoleID, FighterList,LieuInfoList, AtkAdd, HpAdd,TrSpecial,LieuAddAttr).
	
persist_roleBag(RoleID) ->
	{GerEquipList, BagEquip, BagItem} = role_data:get_all_item(),
	db_sql:set_equipList(RoleID, [{0,BagEquip}|GerEquipList]),
	db_sql:set_bagItem(RoleID, BagItem).

persist_role_skin(RoleID) ->
    SkinInfo = role_skin:get_skin_info(),
    db_sql:set_skin_info(RoleID, SkinInfo).

persist_playerTrainingRoom(RoleID) ->
	db_sql:setTrainingRoom(RoleID, role_trainingRoom:getPlayerTrainingRoomData()).

%% ====================================================================
%% Internal functions
%% ====================================================================


