-record(cs_trainingRoom_info,{
	}).
-record(sc_trainingRoom_info,{
	chapterID=[] :: ?int16
	,leftHP=[] :: ?int64}).
-record(cs_trainingRoom_start_training,{
	energy=[] :: ?int16}).
-record(p_ger_view,{
	gerQuality=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerTypeID=[] :: ?int16}).
-record(p_item_view,{
	itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int16
	,itemRank=[] :: ?int8
	,itemNum=[] :: ?int16}).
-record(p_reward_info,{
	coin=[] :: ?int32
	,roleExp=[] :: ?int32
	,gerExp=[] :: ?int32
	,gold=[] :: ?int32
	,reputation=[] :: ?int32
	,itemList=[] :: [#p_item_view{}]
	,gerList=[] :: [#p_ger_view{}]}).
-record(p_trainingRoom_reward,{
	index=[] :: ?int8
	,chapterID=[] :: ?int16
	,coin=[] :: ?int32
	,roleExp=[] :: ?int32
	,rateReward=[] :: [#p_reward_info{}]
	,bossReward=[] :: [#p_reward_info{}]
	,energy=[] :: ?int16}).
-record(sc_trainingRoom_start_training,{
	result=[] :: ?int8
	,rewardList=[] :: [#p_trainingRoom_reward{}]}).
-record(p_diamond,{
	pos=[] :: ?int8
	,diamondID=[] :: ?int16}).
-record(cs_diamond_combine,{
	sourceDiamondID=[] :: ?int16
	,desNum=[] :: ?int16}).
-record(sc_diamond_combine,{
	result=[] :: ?int8}).
-record(cs_diamond_equip,{
	gerUID=[] :: ?int64
	,diamondID=[] :: ?int16
	,pos=[] :: ?int8}).
-record(sc_diamond_equip,{
	result=[] :: ?int8
	,gerUID=0 :: ?int64
	,gerPos=0 :: ?int8
	,diamondID=0 :: ?int16
	,diamondPos=0 :: ?int8}).
-record(cs_diamond_demount,{
	gerUID=[] :: ?int64
	,diamondID=[] :: ?int16
	,pos=[] :: ?int8}).
-record(sc_diamond_demount,{
	result=[] :: ?int8
	,gerUID=0 :: ?int64
	,gerPos=0 :: ?int8
	,diamondID=0 :: ?int16
	,diamondPos=0 :: ?int8}).
-record(cs_diamond_demount_multi,{
	diamondID=[] :: ?int16}).
-record(p_diamond_location,{
	gerUID=[] :: ?int64
	,gerPos=[] :: ?int8
	,diamondPos=[] :: ?int8}).
-record(sc_diamond_demount_multi,{
	result=[] :: ?int8
	,demountLocationList=[] :: [#p_diamond_location{}]}).
-record(cs_diamond_holygrail_info,{
	gerUID=[] :: ?int64}).
-record(sc_diamond_holygrail_info,{
	result=[] :: ?int8
	,holyGrailLevel=0 :: ?int8
	,isFinishSacrifice=false :: boolean()}).
-record(cs_diamond_holygrail_sacrifice,{
	sourceGerUID=[] :: ?int64
	,foodGerUID=[] :: ?int64}).
-record(sc_diamond_holygrail_sacrifice,{
	result=[] :: ?int8}).
-record(cs_diamond_holygrail_uplevel,{
	gerUID=[] :: ?int64}).
-record(sc_diamond_holygrail_uplevel,{
	result=[] :: ?int8
	,gerUID=0 :: ?int64
	,holyGrailLevel=0 :: ?int8}).
-record(cs_conquerisland_sign,{
	}).
-record(sc_conquerisland_sign,{
	result=[] :: ?int8
	,times=[] :: ?int32}).
-record(cs_conquerisland_buy,{
	}).
-record(sc_conquerisland_buy,{
	result=[] :: ?int8
	,cur_times=[] :: ?int16
	,times=[] :: ?int16}).
-record(cs_conquerisland_info,{
	}).
-record(sc_conquerisland_info,{
	times=[] :: ?int16
	,cur_times=[] :: ?int16
	,buy_left=[] :: ?int16
	,plane_level=[] :: ?int16
	,golds=[] :: [?int32]
	,winGas=[] :: ?int32
	,afk_punish=[] :: ?int8
	,punish_timestamp=[] :: ?int32}).
-record(cs_conquerisland_unrequest,{
	}).
-record(sc_conquerisland_unrequest,{
	result=[] :: ?int8}).
-record(sc_conquerisland_times_update,{
	times=[] :: ?int16}).
-record(cs_conquerisland_war_base_info,{
	}).
-record(p_pos,{
	x=0 :: ?int16
	,y=0 :: ?int16}).
-record(p_centre,{
	centreId=[] :: ?int8
	,owner=[] :: ?int8
	,attackerNum=[] :: ?int8
	,defenderNum=[] :: ?int8
	,buffId=[] :: ?int8
	,centrePos=[] :: #p_pos{}
	,ownSec=[] :: ?int32}).
-record(p_player,{
	serverId=[] :: ?int32
	,roleId=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fly=[] :: ?int8
	,fightPower=[] :: ?int64
	,type=[] :: ?int8
	,blood=[] :: ?int8
	,rebornSec=[] :: ?int32
	,startPos=[] :: #p_pos{}
	,endPos=[] :: #p_pos{}
	,centreId=[] :: ?int8
	,startSec=[] :: ?int32
	,speedadd=[] :: ?int16
	,bossfightpower=[] :: ?int64
	,speed=[] :: ?int16}).
-record(p_conquerisland_boss,{
	bossId=[] :: ?int8
	,bossPos=[] :: #p_pos{}
	,bossDemagePlus=[] :: ?int16
	,bossHpAdd=[] :: ?int16
	,bossCritPlus=[] :: ?int16
	,bossDemageDec=[] :: ?int16
	,bossHpMax=[] :: ?int64
	,bossHp=[] :: ?int64
	,bossLevel=[] :: ?int16
	,bossType=[] :: ?int8}).
-record(sc_conquerisland_war_base_info,{
	result=[] :: ?int8
	,endTimeStamp=0 :: ?int32
	,attackerPos=[] :: #p_pos{}
	,defenderPos=[] :: #p_pos{}
	,centre=[] :: [#p_centre{}]
	,players=[] :: [#p_player{}]
	,bosses=[] :: [#p_conquerisland_boss{}]}).
-record(cs_conquerisland_role_dtl,{
	serverId=[] :: ?int32
	,roleId=[] :: ?int32}).
-record(p_carlos_replay_dtl,{
	isRole1Win=[] :: ?int8
	,replayUID=[] :: ?int64
	,role1Name=[] :: ?string
	,role2Name=[] :: ?string}).
-record(p_conquerisland_ger,{
	gerTypeID=[] :: ?int32
	,gerRank=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerMaxHp=[] :: ?int64
	,gerHp=[] :: ?int64}).
-record(sc_conquerisland_role_dtl,{
	result=[] :: ?int8
	,gerList=[] :: [#p_conquerisland_ger{}]
	,replayList=[] :: [#p_carlos_replay_dtl{}]
	,grade=[] :: ?int16}).
-record(cs_conquerisland_mov,{
	centreId=[] :: ?int8}).
-record(sc_conquerisland_mov,{
	result=[] :: ?int8}).
-record(cs_conquerisland_stop,{
	}).
-record(sc_conquerisland_stop,{
	result=[] :: ?int8}).
-record(cs_conquerisland_centre_dtl,{
	centreId=[] :: ?int8}).
-record(sc_conquerisland_centre_dtl,{
	result=[] :: ?int8
	,centre=[] :: #p_centre{}
	,attackers=[] :: [#p_player{}]
	,defenders=[] :: [#p_player{}]}).
-record(cs_conquerisland_attack,{
	serverId=[] :: ?int32
	,roleId=[] :: ?int32
	,centreId=[] :: ?int8}).
-record(p_action,{
	gerPos=[] :: ?int8
	,actionID=[] :: ?int16
	,targetPos=[] :: [?int8]
	,addSp=[] :: ?int8
	,addHp=[] :: ?int64
	,addProHp=[] :: ?int64
	,state=[] :: ?int16}).
-record(p_fighter,{
	gerID=[] :: ?int64
	,gerTypeID=[] :: ?int16
	,gerPos=[] :: ?int8
	,gerHp=[] :: ?int64
	,gerHpMax=[] :: ?int64
	,gerProHp=[] :: ?int64
	,gerProHpMax=[] :: ?int64
	,gerSp=[] :: ?int16
	,spMax=[] :: ?int16
	,gerQuality=[] :: ?int8
	,stoneEffectType=[] :: ?int8
	,maxAwakeStep=[] :: ?int8
	,gerBody=[] :: ?int8
	,damage=[] :: ?int64}).
-record(sc_fight_request,{
	fighterList=[] :: [#p_fighter{}]
	,actionList=[] :: [#p_action{}]
	,result=[] :: boolean()}).
-record(sc_conquerisland_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_conquerisland_centre_occupy,{
	centreId=[] :: ?int8}).
-record(sc_conquerisland_centre_occupy,{
	result=[] :: ?int8
	,sec=[] :: ?int32}).
-record(cs_conquerisland_reborn,{
	}).
-record(sc_conquerisland_reborn,{
	result=[] :: ?int8}).
-record(cs_conquerisland_replay,{
	replayUID=[] :: ?int64}).
-record(sc_conquerisland_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(sc_conquerisland_update_war,{
	updatePlayerList=[] :: [#p_player{}]
	,updateCentreList=[] :: [#p_centre{}]
	,updateBossList=[] :: [#p_conquerisland_boss{}]}).
-record(cs_conquerisland_self,{
	}).
-record(sc_conquerisland_self,{
	self=[] :: #p_player{}}).
-record(cs_conquerisland_rank,{
	}).
-record(p_fight_rank,{
	serverID=[] :: ?int32
	,roleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,killNum=[] :: ?int32
	,centreNum=[] :: ?int32
	,bossDemage=[] :: ?int64
	,honor=[] :: ?int32}).
-record(sc_conquerisland_rank,{
	ranklist=[] :: [#p_fight_rank{}]}).
-record(cs_conquerisland_talk,{
	data=[] :: ?string}).
-record(cs_conquerisland_get_talk,{
	}).
-record(p_carlos_talk,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,data=[] :: ?string
	,ext=0 :: ?int8}).
-record(sc_conquerisland_get_talk,{
	data=[] :: [#p_carlos_talk{}]}).
-record(p_carlos_rank_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,level=[] :: ?int16
	,get=[] :: ?int16
	,score=[] :: ?int16
	,kill=[] :: ?int16
	,name=[] :: ?string
	,type=0 :: ?int8}).
-record(sc_conquerisland_end_war,{
	data=[] :: [#p_carlos_rank_dtl{}]}).
-record(sc_conquerisland_talk,{
	data=[] :: #p_carlos_talk{}}).
-record(cs_recycle_restore,{
	type=[] :: ?int8
	,gerID=[] :: ?int64
	,equipID=[] :: ?int64
	,isRealRecycl=[] :: ?int8}).
-record(sc_recycle_restore,{
	result=[] :: ?int8
	,isRealRecycl=[] :: ?int8
	,gerID=[] :: ?int64
	,equipID=[] :: ?int64
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_crystal_uplevel,{
	crystaltype=[] :: ?int8
	,crystalexpinc=[] :: ?int64
	,gerUID=[] :: ?int64}).
-record(p_crystalinfo,{
	crystaltype=[] :: ?int8
	,crystalquality=[] :: ?int8
	,crystallevel=[] :: ?int16
	,crystalexp=[] :: ?int64
	,crystalrankexp=[] :: ?int64}).
-record(sc_crystal_uplevel,{
	result=[] :: ?int8
	,isneeduprank=[] :: ?int8
	,gerUID=[] :: ?int64
	,crystaltype=[] :: ?int8
	,crystalinfo=[] :: #p_crystalinfo{}}).
-record(cs_crystal_uprank,{
	crystaltype=[] :: ?int8
	,crystalexpinc=[] :: ?int64
	,gerUID=[] :: ?int64}).
-record(sc_crystal_uprank,{
	result=[] :: ?int8
	,isneeduplevel=[] :: ?int8
	,gerUID=[] :: ?int64
	,crystaltype=[] :: ?int8
	,crystalinfo=[] :: #p_crystalinfo{}}).
-record(cs_crystal_info,{
	gerUID=[] :: ?int64}).
-record(sc_crystal_info,{
	result=[] :: ?int8
	,gerUID=[] :: ?int64
	,type=[] :: ?int8
	,crystallist=[] :: [#p_crystalinfo{}]}).
-record(p_crystalinfo_brief,{
	crystaltype=[] :: ?int8
	,crystalquality=[] :: ?int8
	,crystallevel=[] :: ?int16}).
-record(ger_crystalinfo_brief,{
	gerID=[] :: ?int64
	,type=[] :: ?int8
	,crystalbrieflist=[] :: [#p_crystalinfo_brief{}]}).
-record(cs_doublematch_lineup,{
	}).
-record(p_doublematch_team_trainer_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,skinID=[] :: ?int32
	,trSpecialID=[] :: ?int16
	,level=[] :: ?int32}).
-record(p_equip,{
	itemUID=[] :: ?int64
	,itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int16
	,itemRank=[] :: ?int8
	,itemGerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemDecay=[] :: ?int32
	,itemExp=[] :: ?int16
	,itemenchantType=[] :: ?int8
	,itemenchantLevel=[] :: ?int8
	,itemLegendRank=[] :: ?int8}).
-record(p_demage_rate_unit,{
	type=[] :: ?int8
	,normalRate=[] :: ?int32
	,uniqueRate=[] :: ?int32}).
-record(p_awake_info,{
	step=[] :: ?int8
	,current_skillID=[] :: ?int32
	,current_quality=[] :: ?int8
	,recastskillIDlist=[] :: [?int32]}).
-record(p_ger,{
	gerID=0 :: ?int64
	,gerTypeID=0 :: ?int16
	,gerQuality=0 :: ?int16
	,gerLevel=0 :: ?int16
	,gerAttack=0 :: ?int32
	,gerHpMax=0 :: ?int64
	,gerFightPower=0 :: ?int64
	,gerExp=0 :: ?int64
	,gerProMean=0 :: ?int64
	,gerAwakeInfo=[] :: [#p_awake_info{}]
	,gerDemageRate=[] :: [#p_demage_rate_unit{}]
	,gerBody=0 :: ?int8
	,gerSpeed=0 :: ?int16
	,gerHolyGrailLevel=0 :: ?int8}).
-record(p_doublematch_team_ger_info,{
	pos=[] :: ?int32
	,flage=[] :: ?int32
	,ger=[] :: #p_ger{}
	,equip=[] :: [#p_equip{}]}).
-record(p_doublematch_team_member,{
	trainer=[] :: #p_doublematch_team_trainer_info{}
	,gerlist=[] :: [#p_doublematch_team_ger_info{}]}).
-record(sc_doublematch_lineup,{
	result=[] :: ?int8
	,memberlist=[] :: [#p_doublematch_team_member{}]}).
-record(p_ger_pos_unit,{
	gerID=[] :: ?int64
	,pos=[] :: ?int32}).
-record(cs_doublematch_update_lineup,{
	gerposlist=[] :: [#p_ger_pos_unit{}]}).
-record(sc_doublematch_update_lineup,{
	result=[] :: ?int8
	,memberlist=[] :: [#p_doublematch_team_member{}]}).
-record(cs_doublematch_sign,{
	}).
-record(sc_doublematch_sign,{
	result=[] :: ?int8}).
-record(cs_doublematch_sign_cancel,{
	}).
-record(sc_doublematch_sign_cancel,{
	result=[] :: ?int8}).
-record(cs_doublematch_times_buy,{
	}).
-record(sc_doublematch_times_buy,{
	result=[] :: ?int8
	,cur_times=[] :: ?int16
	,next_buy_cost=[] :: ?int32}).
-record(cs_doublematch_roleinfo,{
	}).
-record(sc_doublematch_roleinfo,{
	result=[] :: ?int8
	,grade=[] :: ?int8
	,grade_level=[] :: ?int16
	,score=[] :: ?int32
	,times=[] :: ?int32
	,next_buy_cost=[] :: ?int32
	,timestamp=[] :: ?int32
	,rank=[] :: ?int32
	,max_free_time=[] :: ?int32
	,local_rank=[] :: ?int32
	,before_rank=[] :: ?int32
	,before_grade=[] :: ?int8
	,before_grade_level=[] :: ?int16
	,before_score=[] :: ?int32}).
-record(cs_doublematch_rankinfo,{
	type=[] :: ?int8
	,position=[] :: ?int32
	,length=[] :: ?int32}).
-record(p_doublematch_rank_unit,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,serverID=[] :: ?int32
	,grade=[] :: ?int8
	,gradeLevel=[] :: ?int16
	,score=[] :: ?int32
	,rank=[] :: ?int32
	,score_change=[] :: ?int32
	,vip=[] :: ?int8}).
-record(sc_doublematch_rankinfo,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,position=[] :: ?int32
	,ranklist=[] :: [#p_doublematch_rank_unit{}]
	,length=[] :: ?int32}).
-record(sc_doublematch_quit,{
	result=[] :: ?int8}).
-record(cs_doublematch_record_list,{
	beginindex=[] :: ?int32
	,type=[] :: ?int8
	,length=[] :: ?int32}).
-record(p_record_unit,{
	recordID=[] :: ?int64
	,recordteammember=[] :: [#p_doublematch_rank_unit{}]
	,timestamp=[] :: ?int32}).
-record(sc_doublematch_record_list,{
	result=[] :: ?int8
	,recordlist=[] :: [#p_record_unit{}]
	,beginindex=[] :: ?int32
	,type=[] :: ?int8
	,length=[] :: ?int32}).
-record(cs_doublematch_replay,{
	recordID=[] :: ?int64}).
-record(sc_fight_double_request,{
	fighterList=[] :: [#p_fighter{}]
	,actionList=[] :: [#p_action{}]
	,result=[] :: boolean()}).
-record(sc_doublematch_replay,{
	result=[] :: ?int8
	,fight_info=[] :: #sc_fight_double_request{}
	,dice_numA=[] :: ?int8
	,dice_numB=[] :: ?int8}).
-record(sc_doublematch_fight,{
	rolelist=[] :: [#p_doublematch_rank_unit{}]
	,dice_numA=[] :: ?int8
	,dice_numB=[] :: ?int8
	,fight_info=[] :: #sc_fight_double_request{}
	,score_change=[] :: ?int32}).
-record(cs_goldenegg_use_item,{
	itemTypeID=[] :: ?int32}).
-record(sc_goldenegg_use_item,{
	result=[] :: ?int8
	,itemTypeID=[] :: ?int32
	,useItemTypeIDList=[] :: [?int32]}).
-record(cs_goldenegg_smash,{
	itemTypeID=[] :: ?int32
	,extraitemTypeIDList=[] :: [?int32]}).
-record(p_reward_view,{
	type=[] :: ?int8
	,value=[] :: ?int32}).
-record(sc_goldenegg_smash,{
	result=[] :: ?int8
	,score=[] :: ?int32
	,rewardlist=[] :: [#p_reward_view{}]}).
-record(cs_goldenegg_roleinfo,{
	}).
-record(sc_goldenegg_roleinfo,{
	totalscore=[] :: ?int32
	,validtimestamp=[] :: ?int32
	,useItemTypeIDList=[] :: [?int32]}).
-record(cs_goldenegg_shop,{
	}).
-record(p_goods_unit,{
	goodsID=[] :: ?int32
	,goods=[] :: #p_reward_info{}
	,price=[] :: ?int32
	,moneytype=[] :: ?int8
	,maxbuytimes=[] :: ?int32
	,buytime=[] :: ?int32
	,shopid=[] :: ?int32}).
-record(sc_goldenegg_shop,{
	result=[] :: ?int8
	,goodslist=[] :: [#p_goods_unit{}]
	,validtimestamp=[] :: ?int32}).
-record(cs_goldenegg_open,{
	}).
-record(sc_goldenegg_open,{
	status=[] :: ?int8
	,endtimestamp=[] :: ?int32}).
-record(sc_goldenegg_update_score,{
	totalscore=[] :: ?int32
	,validtimestamp=[] :: ?int32}).
-record(p_member_info,{
	roleID=[] :: ?int32
	,head=[] :: ?int32
	,camp=[] :: ?int8
	,status=[] :: ?int8
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,level=[] :: ?int32}).
-record(sc_matchRoom_init,{
	roomID=[] :: ?int32
	,members=[] :: [#p_member_info{}]
	,close_timestamp=[] :: ?int32}).
-record(sc_matchRoom_ready,{
	roomID=[] :: ?int32
	,roleID=[] :: ?int32}).
-record(sc_matchRoom_cancel,{
	roomID=[] :: ?int32
	,roleID=[] :: ?int32}).
-record(sc_matchRoom_kick,{
	roomID=[] :: ?int32
	,roleID=[] :: ?int32}).
-record(sc_matchRoom_exit,{
	roomID=[] :: ?int32
	,idlist=[] :: [?int32]}).
-record(sc_matchRoom_new,{
	roomID=[] :: ?int32
	,members=[] :: [#p_member_info{}]}).
-record(cs_matchRoom_ready,{
	roomID=[] :: ?int32}).
-record(cs_matchRoom_cancel,{
	roomID=[] :: ?int32}).
-record(cs_matchRoom_exit,{
	roomID=[] :: ?int32}).
-record(sc_matchRoom_close,{
	roomID=[] :: ?int32
	,reason=[] :: ?int8}).
-record(cs_skin_compose,{
	itemTypeID=[] :: ?int32}).
-record(sc_skin_compose,{
	result=[] :: ?int8
	,is_first=[] :: boolean()}).
-record(cs_skin_activate,{
	itemTypeID=[] :: ?int32
	,pos=[] :: ?int8}).
-record(sc_skin_activate,{
	result=[] :: ?int8}).
-record(cs_skin_equip,{
	itemTypeID=[] :: ?int32}).
-record(sc_skin_equip,{
	result=[] :: ?int8}).
-record(cs_skin_demount,{
	}).
-record(sc_skin_demount,{
	result=[] :: ?int8}).
-record(p_skin,{
	itemTypeID=[] :: ?int32
	,pos=[] :: ?int8}).
-record(cs_skin_info,{
	}).
-record(p_skin_buff,{
	ger_attack_plus=0 :: ?int32
	,ger_demage_sub=0 :: ?int32
	,trainer_demage_plus=0 :: ?int32
	,trainer_mark_plus=0 :: ?int8}).
-record(sc_skin_info,{
	skins=[] :: [#p_skin{}]
	,equip=[] :: ?int32
	,totalAdd=[] :: #p_skin_buff{}}).
-record(p_skin_info,{
	equip=[] :: ?int32}).
-record(cs_home_info,{
	}).
-record(c_info,{
	type=[] :: ?int32
	,cur_level=[] :: ?int32
	,max_level=[] :: ?int32}).
-record(sc_home_info,{
	stage=[] :: ?int8
	,constr_list=[] :: [#c_info{}]
	,dounty_need_win_rate=[] :: ?int32}).
-record(cs_home_build,{
	type=[] :: ?int32}).
-record(sc_home_build,{
	result=[] :: ?int8
	,constr_list=[] :: [#c_info{}]}).
-record(cs_home_up_stage,{
	}).
-record(sc_home_up_stage,{
	result=[] :: ?int8}).
-record(c_need,{
	type=[] :: ?int32
	,level=[] :: ?int32}).
-record(cs_home_get_friend,{
	role_id=[] :: ?int32}).
-record(sc_home_get_friend,{
	stage=[] :: ?int8
	,constr_list=[] :: [#c_info{}]
	,level=[] :: ?int32}).
-record(cs_home_task_info,{
	}).
-record(home_task,{
	id=[] :: ?int32
	,quality=[] :: ?int8
	,level=[] :: ?int32
	,tgt_ger_type=[] :: [?int32]
	,timtout_ts=[] :: ?int32
	,finish_time=[] :: ?int32
	,base_reward_num=[] :: ?int32
	,box_reward=[] :: #p_reward_info{}
	,status=[] :: ?int8
	,role_id=[] :: ?int32
	,ger_id=[] :: [?int64]
	,ger_type=[] :: [?int32]
	,ger_quality=[] :: [?int8]
	,ger_level=[] :: [?int32]
	,onwer_role_id=[] :: ?int32
	,task_type=[] :: ?int32
	,role_level=[] :: ?int32
	,owner_name=[] :: ?string
	,atk_name=[] :: ?string}).
-record(sc_home_task_info,{
	task_list=[] :: [#home_task{}]}).
-record(cs_home_task_operate,{
	operate_type=[] :: ?int8
	,task_id=[] :: ?int32
	,ger_id=[] :: [?int64]
	,role_id=[] :: ?int32}).
-record(sc_home_task_operate,{
	result=[] :: ?int8}).
-record(cs_home_bounty_task_info,{
	}).
-record(sc_home_bounty_task_info,{
	task_list=[] :: [#home_task{}]
	,accept_num=[] :: ?int32
	,accept_max=[] :: ?int32}).
-record(cs_home_cirrus_info,{
	role_id=[] :: ?int32}).
-record(cirrus_node,{
	cirrus_index=[] :: ?int32
	,need_role_lvl=[] :: ?int32
	,need_cirrus_lvl=[] :: ?int32
	,is_win=[] :: ?int8
	,doing_timestamp=[] :: ?int32
	,doing_ger_id=[] :: ?int64
	,cost_type=[] :: ?int8
	,cirrus_reward=[] :: #p_reward_info{}
	,need_energy=[] :: ?int32
	,need_diamond=[] :: ?int32
	,doing_ger_type=[] :: ?int32
	,doing_ger_quality=[] :: ?int32}).
-record(sc_home_cirrus_info,{
	role_id=[] :: ?int32
	,cirrus_node_list=[] :: [#cirrus_node{}]}).
-record(cs_home_cirrus_fight,{
	cirrus_index=[] :: ?int32}).
-record(sc_home_cirrus_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_home_cirrus_operate,{
	operate_type=[] :: ?int32
	,cirrus_index=[] :: ?int32
	,ger_id=[] :: ?int64}).
-record(sc_home_cirrus_operate,{
	result=[] :: ?int8}).
-record(cs_home_exchange,{
	wood_num=[] :: ?int32
	,stone_num=[] :: ?int32}).
-record(sc_home_exchange,{
	result=[] :: ?int8}).
-record(cs_sign_reward_info,{
	}).
-record(p_sign_reward,{
	sign_count=[] :: ?int8
	,sign_reward=[] :: [#p_reward_view{}]
	,is_get_reward=[] :: ?int8}).
-record(sc_sign_reward_info,{
	result=[] :: ?int8
	,rewardlist=[] :: [#p_sign_reward{}]
	,accrewardlist=[] :: [#p_sign_reward{}]
	,signcount=[] :: ?int8
	,is_get_sign_reward=[] :: ?int8}).
-record(cs_sign_reward,{
	rewardtype=[] :: ?int8
	,rewardcount=[] :: ?int8}).
-record(sc_sign_reward,{
	result=[] :: ?int8
	,rewardtype=[] :: ?int8
	,rewardcount=[] :: ?int8
	,sign_reward=[] :: [#p_reward_view{}]}).
-record(cs_awake_ger,{
	gerID=[] :: ?int64
	,step=[] :: ?int8}).
-record(sc_awake_ger,{
	result=[] :: ?int8
	,skillID=[] :: ?int32
	,gerID=[] :: ?int64
	,step=[] :: ?int8}).
-record(cs_awake_recast_ger,{
	gerID=[] :: ?int64
	,step=[] :: ?int8
	,recasttimes=[] :: ?int8}).
-record(sc_awake_recast_ger,{
	result=[] :: ?int8
	,skillID=[] :: [?int32]
	,gerID=[] :: ?int64
	,step=[] :: ?int8}).
-record(cs_awake_exchange_skill,{
	gerID=[] :: ?int64
	,step=[] :: ?int8
	,skillID=[] :: ?int32}).
-record(sc_awake_exchange_skill,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,step=[] :: ?int8
	,skillID=[] :: ?int32}).
-record(cs_awake_cancel_skill,{
	gerID=[] :: ?int64
	,step=[] :: ?int8}).
-record(sc_awake_cancel_skill,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,step=[] :: ?int8}).
-record(cs_generalteam_create,{
	tarRoleID=[] :: ?int32
	,teamType=[] :: ?int8}).
-record(sc_generalteam_create,{
	result=[] :: ?int8}).
-record(cs_generalteam_invite,{
	tarRoleID=[] :: ?int32}).
-record(sc_generalteam_invite,{
	result=[] :: ?int8}).
-record(p_team_member_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,head=[] :: ?int32
	,title=[] :: ?int8
	,level=[] :: ?int16
	,fightPower=[] :: ?int64
	,position=[] :: ?int32}).
-record(sc_generalteam_invite_request,{
	memberinfo=[] :: #p_team_member_info{}
	,teamType=[] :: ?int8
	,teamStatu=[] :: ?int8
	,teamID=[] :: ?int64
	,endtime=[] :: ?int64}).
-record(cs_generalteam_invite_response,{
	inviteAccept=[] :: ?int8
	,teamID=[] :: ?int64
	,inviteRoleID=[] :: ?int32
	,teamType=[] :: ?int8
	,teamStatu=[] :: ?int8}).
-record(sc_generalteam_invite_response,{
	inviteAccept=[] :: ?int8
	,teamID=[] :: ?int64
	,inviteRoleID=[] :: ?int32
	,invitedRole=[] :: #p_team_member_info{}}).
-record(p_team_info,{
	teamType=[] :: ?int8
	,teamID=[] :: ?int64
	,leaderID=[] :: ?int32
	,viceleaderIDList=[] :: [?int32]
	,memberList=[] :: [#p_team_member_info{}]}).
-record(update_generalteam_info,{
	team_statu=[] :: ?int8
	,team_info=[] :: #p_team_info{}}).
-record(cs_generalteam_leaveteam,{
	}).
-record(sc_generalteam_leaveteam,{
	result=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(cs_generalteam_disbandteam,{
	}).
-record(sc_generalteam_disbandteam,{
	result=[] :: ?int8}).
-record(cs_generalteam_kick,{
	kickRoleID=[] :: ?int32}).
-record(sc_generalteam_kick,{
	result=[] :: ?int8
	,roleID=[] :: ?int32
	,kickRoleID=[] :: ?int32}).
-record(cs_generalteam_change_authority,{
	type=[] :: ?int8
	,tarRoleID=[] :: ?int32}).
-record(sc_generalteam_change_authority,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,roleID=[] :: ?int32
	,tarRoleID=[] :: ?int32}).
-record(cs_generalteam_talk,{
	message=[] :: ?string}).
-record(sc_generalteam_talk,{
	result=[] :: ?int8}).
-record(update_generalteam_talk,{
	messageRole=[] :: #p_team_member_info{}
	,message=[] :: ?string}).
-record(cs_generalteam_info,{
	}).
-record(sc_generalteam_info,{
	result=[] :: ?int8
	,teamInfo=[] :: #p_team_info{}}).
-record(cs_generalteam_change_teamtype,{
	oldTeamType=[] :: ?int8
	,newTeamType=[] :: ?int8}).
-record(sc_generalteam_change_teamtype,{
	result=[] :: ?int8
	,newTeamType=[] :: ?int8}).
-record(cs_panicbuy_info,{
	}).
-record(p_ger_unit,{
	gertypeID=[] :: ?int32
	,gernum=[] :: ?int32
	,gerlevel=[] :: ?int32
	,gerquality=[] :: ?int32}).
-record(p_item_unit,{
	itemtypeID=[] :: ?int32
	,itemnum=[] :: ?int32
	,itemlevel=[] :: ?int32
	,itemrank=[] :: ?int32}).
-record(p_sell_reward_unit,{
	goldnum=[] :: ?int64
	,coinnum=[] :: ?int64
	,reputationnum=[] :: ?int64
	,itemlist=[] :: [#p_item_unit{}]
	,gerlist=[] :: [#p_ger_unit{}]}).
-record(p_panic_buy_config,{
	id=[] :: ?int32
	,needconfig=[] :: #p_sell_reward_unit{}
	,rewardconfig=[] :: #p_sell_reward_unit{}
	,buytime=[] :: ?int32
	,personbuytime=[] :: ?int32
	,totaltime=[] :: ?int32
	,begintime=[] :: ?int64
	,endtime=[] :: ?int64}).
-record(sc_panicbuy_info,{
	result=[] :: ?int8
	,panicbuyinfo=[] :: [#p_panic_buy_config{}]
	,title=[] :: ?string}).
-record(cs_panicbuy_once,{
	id=[] :: ?int32}).
-record(sc_panicbuy_once,{
	result=[] :: ?int8
	,rewardList=[] :: [#p_reward_view{}]}).
-record(cs_discount_activity_list,{
	}).
-record(p_activity_unit,{
	activityID=[] :: ?int32
	,real_activityID=[] :: ?int32
	,begintime=[] :: ?int64
	,endtime=[] :: ?int64
	,activityType=[] :: ?int8
	,activitypay=[] :: ?int32}).
-record(sc_discount_activity_list,{
	result=[] :: ?int8
	,activitylist=[] :: [#p_activity_unit{}]}).
-record(cs_discount_exchange_info,{
	}).
-record(p_exchange_config_unit,{
	id=[] :: ?int32
	,needconfig=[] :: #p_sell_reward_unit{}
	,getconfig=[] :: #p_sell_reward_unit{}
	,times=[] :: ?int32
	,totaltimes=[] :: ?int32}).
-record(sc_discount_exchange_info,{
	result=[] :: ?int8
	,title=[] :: ?string
	,configlist=[] :: [#p_exchange_config_unit{}]
	,real_activityID=[] :: ?int32}).
-record(cs_discount_exchange,{
	real_activityID=[] :: ?int32
	,id=[] :: ?int32}).
-record(sc_discount_exchange,{
	result=[] :: ?int8
	,times=[] :: ?int32
	,id=[] :: ?int32
	,rewardList=[] :: [#p_reward_view{}]}).
-record(cs_discount_pay_activity_info,{
	}).
-record(p_pay_config_unit,{
	id=[] :: ?int32
	,payrank=[] :: ?int32
	,getconfig=[] :: #p_sell_reward_unit{}
	,times=[] :: ?int32
	,totaltimes=[] :: ?int32}).
-record(sc_discount_pay_activity_info,{
	result=[] :: ?int8
	,title=[] :: ?string
	,configlist=[] :: [#p_pay_config_unit{}]
	,real_activityID=[] :: ?int32}).
-record(cs_discount_pay_activity,{
	real_activityID=[] :: ?int32
	,id=[] :: ?int32}).
-record(sc_discount_pay_activity,{
	result=[] :: ?int8
	,times=[] :: ?int32
	,id=[] :: ?int32
	,rewardList=[] :: [#p_reward_view{}]}).
-record(p_homestead_log,{
	roleName=[] :: ?string
	,type=[] :: ?int8
	,machine=0 :: ?int8
	,gerName=[] :: ?string
	,add=[] :: ?int8
	,second=[] :: ?int32}).
-record(p_homestead_machine,{
	num=[] :: ?int8
	,seedItemID=0 :: ?int16
	,endSecond=0 :: ?int32
	,harvest=0 :: ?int32
	,add4Energy=0 :: ?int32
	,addEnergyEndS=0 :: ?int32
	,get_count=0 :: ?int8
	,startSecond=1 :: ?int32}).
-record(p_homestead,{
	roleName=[] :: ?string
	,energyTimes=[] :: ?int8
	,matingTimes=[] :: ?int8
	,matingCoolSecond=0 :: ?int32
	,add4mating=0 :: ?int8
	,gerTypeID=0 :: ?int16
	,gerID=0 :: ?int64
	,quality=0 :: ?int16
	,level=0 :: ?int16
	,refreshMatingSecond=0 :: ?int32}).
-record(sc_homestead_error,{
	reason_code=[] :: ?int8}).
-record(cs_homestead_get_info,{
	}).
-record(sc_homestead_get_info,{
	baseinfo=[] :: #p_homestead{}
	,machineList=[] :: [#p_homestead_machine{}]}).
-record(cs_homestead_get_friend_info,{
	roleID=[] :: ?int32}).
-record(sc_homestead_get_friend_info,{
	roleID=[] :: ?int32
	,baseinfo=[] :: #p_homestead{}
	,machineList=[] :: [#p_homestead_machine{}]}).
-record(cs_homestead_unlock_machine,{
	num=[] :: ?int8}).
-record(sc_homestead_unlock_machine,{
	machine=[] :: #p_homestead_machine{}}).
-record(cs_homestead_uproot_seed,{
	num=[] :: ?int8}).
-record(sc_homestead_uproot_seed,{
	num=[] :: ?int8}).
-record(cs_homestead_harvest,{
	num=[] :: ?int8}).
-record(sc_homestead_harvest,{
	updata_machine=[] :: #p_homestead_machine{}
	,reward=[] :: #p_reward_view{}}).
-record(cs_homestead_seeding,{
	num=[] :: ?int8
	,seedItemID=[] :: ?int16}).
-record(sc_homestead_seeding,{
	updata_machine=[] :: #p_homestead_machine{}}).
-record(sc_homestead_update_machine,{
	updata_machine=[] :: #p_homestead_machine{}}).
-record(cs_homestead_change_ger,{
	gerID=[] :: ?int64}).
-record(sc_homestead_change_ger,{
	gerID=[] :: ?int64}).
-record(cs_homestead_mating,{
	roleID=[] :: ?int32}).
-record(sc_homestead_mating,{
	matingTimes=[] :: ?int8
	,rewardList=[] :: [#p_reward_view{}]
	,fRoleID=[] :: ?int32
	,matingCoolSecond=[] :: ?int32
	,add4mating=[] :: ?int8}).
-record(sc_homestead_mating_to_friend,{
	log=[] :: #p_homestead_log{}
	,matingCoolSecond=[] :: ?int32
	,add4mating=[] :: ?int8}).
-record(cs_homestead_addenergy,{
	roleID=[] :: ?int32
	,num=[] :: ?int8}).
-record(sc_homestead_addenergy,{
	roleID=[] :: ?int32
	,num=[] :: ?int8
	,energyTimes=[] :: ?int8
	,add4Energy=[] :: ?int32
	,addEnergyEndS=[] :: ?int32
	,rewardList=[] :: [#p_reward_view{}]}).
-record(sc_homestead_addenergy_to_friend,{
	num=[] :: ?int8
	,log=[] :: #p_homestead_log{}
	,addEnergyEndS=[] :: ?int32
	,add4Energy=[] :: ?int32}).
-record(cs_homestead_get_log,{
	}).
-record(sc_homestead_get_log,{
	list=[] :: [#p_homestead_log{}]}).
-record(cs_homestead_get_friend_log,{
	roleID=[] :: ?int32}).
-record(sc_homestead_get_friend_log,{
	roleID=[] :: ?int32
	,list=[] :: [#p_homestead_log{}]}).
-record(sc_homestead_sync_mating_cool_second,{
	roleID=[] :: ?int32
	,matingCoolSecond=[] :: ?int32}).
-record(sc_homestead_sync_ger,{
	roleID=[] :: ?int32
	,gerTypeID=[] :: ?int32
	,gerQuality=[] :: ?int16}).
-record(sc_homestead_sync_add_enagy,{
	roleID=[] :: ?int32
	,beginGold=0 :: ?int32
	,endGold=0 :: ?int32
	,beginBadge=0 :: ?int32
	,endBadge=0 :: ?int32}).
-record(cs_homestead_compose,{
	itemTypeID=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_homestead_compose,{
	result=[] :: ?int8}).
-record(cs_homestead_compose_list,{
	}).
-record(p_compose_info,{
	costTypeID=[] :: ?int16
	,needNum=[] :: ?int16
	,getNum=[] :: ?int16
	,targetID=[] :: ?int16}).
-record(sc_homestead_compose_list,{
	infolist=[] :: [#p_compose_info{}]}).
-record(p_task,{
	task_id=[] :: ?int32
	,status=[] :: ?int8
	,trigger_num=[] :: ?int32}).
-record(cs_task_get_info,{
	}).
-record(sc_task_get_info,{
	main_task_list=[] :: [#p_task{}]
	,today_task_list=[] :: [#p_task{}]
	,ach_task_list=[] :: [#p_task{}]
	,family_today_task_list=[] :: [#p_task{}]
	,wor_task_list=[] :: [#p_task{}]
	,activity_task_list=[] :: [#p_task{}]}).
-record(cs_task_operate,{
	operate_code=[] :: ?int8
	,task_id=[] :: ?int32}).
-record(sc_task_operate,{
	task_type=[] :: ?int8
	,task_id=[] :: ?int32
	,status=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(sc_task_error,{
	result=[] :: ?int8}).
-record(sc_task_notify_change,{
	task_type=[] :: ?int8
	,updata_task_list=[] :: [#p_task{}]
	,del_task_id=[] :: [?int32]}).
-record(cs_fight_request,{
	}).
-record(cs_consumerank_info,{
	}).
-record(sc_consumerank_info,{
	status=[] :: ?int8
	,finishtime=[] :: ?int32}).
-record(cs_consumerank_list,{
	type=[] :: ?int8}).
-record(p_consumerank_unit,{
	roleID=0 :: ?int32
	,isMale=[] :: boolean()
	,level=0 :: ?int16
	,title=0 :: ?int8
	,roleName=[] :: ?string
	,fightPower=0 :: ?int64
	,rank=0 :: ?int16
	,head=0 :: ?int32
	,vip=0 :: ?int8
	,value=0 :: ?int32}).
-record(sc_consumerank_list,{
	type=[] :: ?int8
	,list=[] :: [#p_consumerank_unit{}]
	,role_unit=[] :: #p_consumerank_unit{}}).
-record(cs_buddy_partner_insert,{
	insertGerID=[] :: ?int64
	,replaceGerID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_buddy_partner_insert,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,insertGerID=[] :: ?int64
	,replaceGerID=[] :: ?int64}).
-record(cs_buddy_partner_insert_batch,{
	type=[] :: ?int8
	,insertGerIDList=[] :: [?int64]}).
-record(sc_buddy_partner_insert_batch,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,insertGerIDList=[] :: [?int64]}).
-record(cs_buddy_partner_remove,{
	type=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_buddy_partner_remove,{
	result=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(p_dSign_unit,{
	monStartDate=[] :: ?int32
	,signData=[] :: ?int32
	,totalSign=[] :: ?int16
	,getReward=[] :: ?int8}).
-record(cs_dSign_info,{
	}).
-record(sc_dSign_info,{
	birthDate=[] :: ?int32
	,sevenConDays=[] :: ?int32
	,signTotal=[] :: ?int32
	,unit=[] :: [#p_dSign_unit{}]
	,isMonSignToday=[] :: ?int8
	,lastMonSignDate=[] :: ?int32
	,monReward=[] :: [#p_reward_view{}]}).
-record(cs_dSign_sign,{
	}).
-record(sc_dSign_sign,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_dSign_reSign,{
	monStartDate=[] :: ?int32
	,day=[] :: ?int16}).
-record(sc_dSign_reSign,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_dSign_get_sevenReward,{
	}).
-record(sc_dSign_get_sevenReward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_dSign_get_monReward,{
	}).
-record(sc_dSign_get_monReward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_dSign_mark_mon,{
	}).
-record(sc_dSign_mark_mon,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_trainerRear_brief,{
	}).
-record(sc_trainerRear_brief,{
	produceState=1 :: ?int8
	,incubationState=1 :: ?int8}).
-record(cs_trainerRear_info,{
	type=[] :: ?int8}).
-record(p_object_unit,{
	objectID=0 :: ?int32
	,quality=0 :: ?int8}).
-record(p_rear_machine,{
	machineID=0 :: ?int8
	,type=0 :: ?int8
	,objectList=[] :: [#p_object_unit{}]
	,matureTime=0 :: ?int32
	,totalTime=0 :: ?int32}).
-record(sc_trainerRear_info,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,rearList=[] :: [#p_rear_machine{}]}).
-record(cs_trainerRear_insert,{
	machineID=[] :: ?int8
	,type=[] :: ?int8
	,objectIDList=[] :: [?int64]}).
-record(sc_trainerRear_insert,{
	result=[] :: ?int8
	,machine=[] :: #p_rear_machine{}}).
-record(cs_trainerRear_mature,{
	type=[] :: ?int8
	,machineID=[] :: ?int8}).
-record(sc_trainerRear_mature,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,machineID=[] :: ?int8
	,gain=[] :: [#p_reward_view{}]}).
-record(cs_trainerRear_accelerate,{
	type=[] :: ?int8
	,machineID=[] :: ?int8
	,accelerateMin=[] :: ?int16}).
-record(sc_trainerRear_accelerate,{
	result=[] :: ?int8
	,type=0 :: ?int8
	,machineID=0 :: ?int8
	,matureTime=0 :: ?int32}).
-record(cs_dojangrank_info,{
	}).
-record(sc_dojangrank_info,{
	challenge_time=[] :: ?int32
	,buy_cost=[] :: ?int32
	,already_buy_time=[] :: ?int32
	,can_buy_time=[] :: ?int32}).
-record(cs_dojangrank_rank,{
	index=[] :: ?int32}).
-record(p_dojangrank_rank,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,vip=[] :: ?int8
	,rank=[] :: ?int32
	,selected_ger_type=[] :: ?int32}).
-record(sc_dojangrank_rank,{
	index=[] :: ?int32
	,p_dojangrank_rank_self=[] :: #p_dojangrank_rank{}
	,p_dojangrank_rank_list=[] :: [#p_dojangrank_rank{}]}).
-record(cs_dojangrank_buy,{
	buy_time=[] :: ?int32}).
-record(sc_dojangrank_buy,{
	result=[] :: ?int32
	,new_buy_cost=[] :: ?int32
	,already_buy_time=[] :: ?int32
	,can_buy_time=[] :: ?int32}).
-record(cs_dojangrank_select_ger_type,{
	index=[] :: ?int32
	,selected_ger_type=[] :: ?int32}).
-record(sc_dojangrank_select_ger_type,{
	index=[] :: ?int32
	,selected_ger_type=[] :: ?int32
	,result=[] :: ?int32}).
-record(cs_dojangrank_fight,{
	index=[] :: ?int32
	,enemy_role_id=[] :: ?int32
	,rank=[] :: ?int32}).
-record(sc_dojangrank_fight,{
	result=[] :: ?int8
	,p_dojangrank_rank_self=[] :: #p_dojangrank_rank{}
	,p_dojangrank_rank_list=[] :: [#p_dojangrank_rank{}]
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: #p_reward_info{}}).
-record(cs_dojangrank_replay_list,{
	}).
-record(p_dojang_replay_info,{
	attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,attackerNewRank=[] :: ?int16
	,defenderNewRank=[] :: ?int16
	,replayUID=[] :: ?int64
	,time=[] :: ?int32
	,rank_type=[] :: ?int32
	,is_win=[] :: ?int32}).
-record(sc_dojangrank_replay_list,{
	infoList=[] :: [#p_dojang_replay_info{}]}).
-record(cs_dojangrank_replay_detail,{
	replayUID=[] :: ?int64}).
-record(sc_dojangrank_replay_detail,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_dojangrank_ger_view_other,{
	tarRoleID=[] :: ?int32
	,rank=[] :: ?int32}).
-record(sc_dojangrank_ger_view_other,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger{}]}).
-record(cs_dojangrank_self_rank,{
	}).
-record(sc_dojangrank_self_rank,{
	rank_num_list=[] :: [?int32]}).
-record(cs_dojangrank_world_info,{
	}).
-record(sc_dojangrank_world_info,{
	challenge_time=[] :: [?int32]
	,buy_cost=[] :: ?int32
	,already_buy_time=[] :: [?int32]
	,can_buy_time=[] :: [?int32]
	,open_state=[] :: ?int32
	,next_timestamp=[] :: ?int32
	,attend_flag=[] :: ?int32
	,session_num=[] :: ?int32}).
-record(cs_dojangrank_world_rank,{
	index=[] :: ?int32
	,start=[] :: ?int32
	,length=[] :: ?int32
	,type=[] :: ?int32}).
-record(dr_ger_info,{
	ger_type=[] :: ?int32
	,ger_quality=[] :: ?int32
	,ger_level=[] :: ?int32}).
-record(p_dr_world_rank,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,vip=[] :: ?int8
	,rank=[] :: ?int32
	,selected_ger_type=[] :: ?int32
	,skinID=[] :: ?int32
	,serverID=[] :: ?int32
	,ger_info_list=[] :: [#dr_ger_info{}]
	,win_num=[] :: ?int32
	,lost_num=[] :: ?int32
	,total_num=[] :: ?int32
	,rank_score=[] :: ?int32}).
-record(sc_dojangrank_world_rank,{
	index=[] :: ?int32
	,p_dojangrank_rank_self=[] :: #p_dr_world_rank{}
	,p_dojangrank_rank_list=[] :: [#p_dr_world_rank{}]}).
-record(cs_dojangrank_world_buy,{
	rank_type=[] :: ?int32
	,buy_time=[] :: ?int32}).
-record(sc_dojangrank_world_buy,{
	result=[] :: ?int32
	,rank_type=[] :: ?int32
	,new_buy_cost=[] :: ?int32
	,already_buy_time=[] :: ?int32
	,can_buy_time=[] :: ?int32}).
-record(cs_dojangrank_world_refresh_enemy,{
	rank_type=[] :: ?int32
	,refresh_type=[] :: ?int32}).
-record(sc_dojangrank_world_refresh_enemy,{
	enemy_list=[] :: [#p_dr_world_rank{}]
	,refresh_cost=[] :: ?int32
	,p_dojangrank_rank_self=[] :: #p_dr_world_rank{}
	,result=[] :: ?int32}).
-record(cs_dojangrank_world_select_ger_type,{
	index=[] :: ?int32
	,selected_ger_type=[] :: ?int32}).
-record(sc_dojangrank_world_select_ger_type,{
	index=[] :: ?int32
	,selected_ger_type=[] :: ?int32
	,result=[] :: ?int32}).
-record(cs_dojangrank_world_fight,{
	index=[] :: ?int32
	,enemy_role_id=[] :: ?int32
	,rank=[] :: ?int32}).
-record(sc_dojangrank_world_fight,{
	result=[] :: ?int8
	,p_dojangrank_rank_self=[] :: #p_dr_world_rank{}
	,p_dojangrank_rank_list=[] :: [#p_dr_world_rank{}]
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: #p_reward_info{}}).
-record(cs_dojangrank_world_replay_list,{
	index=[] :: ?int32}).
-record(p_dr_dojang_replay_info,{
	attacker_info=[] :: #p_dr_world_rank{}
	,defender_info=[] :: #p_dr_world_rank{}
	,replayUID=[] :: ?int64
	,time=[] :: ?int32
	,rank_type=[] :: ?int32
	,is_win=[] :: ?int32}).
-record(sc_dojangrank_world_replay_list,{
	infoList=[] :: [#p_dr_dojang_replay_info{}]}).
-record(cs_dojangrank_world_replay_detail,{
	replayUID=[] :: ?int64
	,replay_type=[] :: ?int32}).
-record(sc_dojangrank_world_replay_detail,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_dojangrank_world_ger_view_other,{
	tarRoleID=[] :: ?int32
	,rank_index=[] :: ?int32
	,rank_data_type=[] :: ?int32}).
-record(sc_dojangrank_world_ger_view_other,{
	role_info=[] :: #p_dr_world_rank{}
	,ger_info_list=[] :: [#dr_ger_info{}]}).
-record(cs_dojangrank_world_top_replay_list,{
	index=[] :: ?int32}).
-record(sc_dojangrank_world_top_replay_list,{
	infoList=[] :: [#p_dr_dojang_replay_info{}]}).
-record(cs_trainerProf_info,{
	}).
-record(p_trainerProf_unit,{
	type=[] :: ?int8
	,level=[] :: ?int8}).
-record(sc_trainerProf_info,{
	prof=[] :: [#p_trainerProf_unit{}]}).
-record(cs_trainerProf_uplevel,{
	type=[] :: ?int8}).
-record(sc_trainerProf_uplevel,{
	result=[] :: ?int8
	,newLevel=0 :: ?int8
	,type=[] :: ?int8}).
-record(cs_trainerProf_battle_info,{
	}).
-record(p_trainerProf_battle_unit,{
	tagID=[] :: ?int8
	,level=[] :: ?int16}).
-record(sc_trainerProf_battle_info,{
	battleInfo=[] :: [#p_trainerProf_battle_unit{}]}).
-record(cs_trainerProf_battle_uplevel,{
	tagID=[] :: ?int8}).
-record(sc_trainerProf_battle_uplevel,{
	result=[] :: ?int8
	,tagID=[] :: ?int8}).
-record(cs_trainerProf_battle_unclock,{
	tagID=[] :: ?int8}).
-record(sc_trainerProf_battle_unclock,{
	result=[] :: ?int8
	,tagID=[] :: ?int8}).
-record(cs_treasurebowl_info,{
	}).
-record(p_treasurebowl_draw,{
	drawID=0 :: ?int32
	,state=0 :: ?int8
	,period=[] :: ?int8
	,reward=[] :: #p_reward_info{}}).
-record(p_treasurebowl_activity,{
	activityID=0 :: ?int32
	,activitystate=0 :: ?int8
	,drawlist=[] :: [#p_treasurebowl_draw{}]
	,type=0 :: ?int8
	,condition=0 :: ?int32}).
-record(sc_treasurebowl_info,{
	result=[] :: ?int8
	,activitylist=[] :: [#p_treasurebowl_activity{}]
	,period=0 :: ?int8
	,periodendtime=[] :: [?int32]
	,activitybegin=0 :: ?int32
	,activityend=0 :: ?int32
	,content=[] :: ?string}).
-record(sc_treasurebowl_update,{
	period=[] :: ?int8}).
-record(cs_treasurebowl_exchange,{
	activityID=[] :: ?int32}).
-record(sc_treasurebowl_exchange,{
	result=[] :: ?int8}).
-record(cs_treasurebowl_draw,{
	activityID=[] :: ?int32
	,drawID=[] :: ?int32}).
-record(sc_treasurebowl_draw,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(sc_treasurebowl_open,{
	state=[] :: ?int8}).
-record(cs_maintask_info,{
	}).
-record(p_task_unit,{
	taskID=[] :: ?int32
	,taskFinish=0 :: ?int64
	,taskState=0 :: ?int8}).
-record(sc_maintask_info,{
	maintask=[] :: #p_task_unit{}}).
-record(cs_maintask_draw,{
	taskID=[] :: ?int32}).
-record(sc_maintask_draw,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_xbattle_info,{
	}).
-record(sc_xbattle_info,{
	chapterID=[] :: ?int16
	,buyQuickCount=[] :: ?int32
	,quickFightSec=[] :: ?int32
	,buyQuickToday=[] :: ?int8
	,coin=[] :: ?int32
	,exp=[] :: ?int32
	,raidTimes=[] :: ?int16}).
-record(cs_xbattle_challenge,{
	chapterID=[] :: ?int16}).
-record(sc_xbattle_challenge,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]
	,dungeonID=[] :: ?int16
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_xbattle_raid,{
	chapterID=[] :: ?int16
	,tryTimes=[] :: ?int16}).
-record(p_xbattle_raid,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]
	,dungeonID=[] :: ?int16}).
-record(sc_xbattle_raid,{
	result=[] :: ?int8
	,raidData=[] :: [#p_xbattle_raid{}]}).
-record(cs_xbattle_offline_reward,{
	}).
-record(sc_xbattle_offline_reward,{
	offlineSec=[] :: ?int32
	,reward=[] :: [#p_reward_info{}]
	,triCount=[] :: ?int16}).
-record(sc_xbattle_tri,{
	triID=[] :: ?int16
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_xbattle_use_elixir,{
	itemUID=[] :: ?int64
	,useNum=[] :: ?int8}).
-record(sc_xbattle_use_elixir,{
	result=[] :: ?int8
	,triCount=[] :: ?int16
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_xbattle_buy_quick,{
	}).
-record(sc_xbattle_buy_quick,{
	result=[] :: ?int8
	,quickFightSec=[] :: ?int32
	,triCount=[] :: ?int32
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_xbattle_get_pass_reward,{
	}).
-record(sc_xbattle_get_pass_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]
	,newChapterID=[] :: ?int16}).
-record(cs_xbattle_chapter_info,{
	chapterID=[] :: ?int16}).
-record(p_xbattle_dungeon,{
	dungeonID=[] :: ?int16
	,times=[] :: ?int8}).
-record(sc_xbattle_chapter_info,{
	isGetReward=[] :: ?int8
	,passDungeons=[] :: [#p_xbattle_dungeon{}]
	,coin=[] :: ?int32
	,exp=[] :: ?int32}).
-record(cs_xbattle_set_reward_chapter,{
	chapterID=[] :: ?int16}).
-record(sc_xbattle_set_reward_chapter,{
	result=[] :: ?int8}).
-record(cs_xbattle_start_reward,{
	}).
-record(sc_xbattle_start_reward,{
	result=[] :: ?int8}).
-record(cs_xbattle_challenge_boss,{
	}).
-record(sc_xbattle_challenge_boss,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_xbattle_package_full,{
	}).
-record(cs_xbattle_package_ok,{
	}).
-record(cs_exBoss_get_info,{
	}).
-record(p_exBoss_dtl,{
	bossID=[] :: ?int16
	,bossLevel=[] :: ?int16
	,isGetReward=[] :: ?int8
	,bossMaxHp=[] :: ?int64
	,bossHp=[] :: ?int64
	,reward=[] :: #p_reward_info{}}).
-record(p_exBoss_times_dtl,{
	haveTimes=[] :: ?int16
	,canBuyTimes=[] :: ?int16
	,nextRefSec=[] :: ?int32
	,freeRef=[] :: ?int8}).
-record(p_exBoss_hit_list,{
	hit=[] :: ?int16
	,harm=[] :: ?int64}).
-record(sc_exBoss_get_info,{
	boss=[] :: #p_exBoss_dtl{}
	,hit=[] :: [#p_exBoss_hit_list{}]
	,times=[] :: #p_exBoss_times_dtl{}
	,nowHit=[] :: ?int16
	,maxHit=[] :: ?int16
	,oneHit=[] :: ?int64}).
-record(cs_exBoss_hit,{
	}).
-record(sc_exBoss_hit,{
	result=[] :: ?int8}).
-record(cs_exBoss_buy_times,{
	}).
-record(sc_exBoss_buy_times,{
	result=[] :: ?int8
	,times=[] :: #p_exBoss_times_dtl{}}).
-record(cs_exBoss_get_reward,{
	}).
-record(sc_exBoss_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]
	,info=[] :: [#sc_exBoss_get_info{}]}).
-record(sc_exBoss_update_times,{
	times=[] :: ?int16}).
-record(cs_exBoss_refresh,{
	}).
-record(sc_exBoss_refresh,{
	result=[] :: ?int8
	,info=[] :: [#sc_exBoss_get_info{}]}).
-record(cs_exBoss_buy_cost,{
	}).
-record(sc_exBoss_buy_cost,{
	buyTimesCost=[] :: ?int16
	,refreshCost=[] :: ?int16
	,sec=[] :: ?int16
	,freeRefTime=[] :: ?int16}).
-record(cs_exBoss_oneKey,{
	}).
-record(sc_exBoss_oneKey,{
	result=[] :: ?int8}).
-record(cs_tasklink_get_info,{
	}).
-record(p_level_info,{
	diffcult_level=[] :: ?int32
	,need_force_value=[] :: ?int32
	,reward_view=[] :: [#p_reward_view{}]}).
-record(sc_tasklink_get_info,{
	sign_status=[] :: ?int32
	,diffcult_level=[] :: ?int32
	,sign_time=[] :: ?int32
	,pay_count=[] :: ?int32
	,level_info_list=[] :: [#p_level_info{}]
	,self_force_value=[] :: ?int32
	,buy_need_gold=[] :: ?int32
	,sign_open_level=[] :: ?int32}).
-record(cs_tasklink_get_progress,{
	}).
-record(p_point_info,{
	point_index=[] :: ?int32
	,point_type=[] :: ?int32
	,point_status=[] :: ?int32
	,point_args=[] :: [?int32]}).
-record(sc_tasklink_get_progress,{
	current_timestamp=[] :: ?int32
	,next_point=[] :: ?int32
	,remaining_time=[] :: ?int32
	,remaining_percent=[] :: ?int32
	,point_info_list=[] :: [#p_point_info{}]
	,end_timestamp=[] :: ?int32
	,members=[] :: [#p_member_info{}]
	,all_need_time=[] :: ?int32}).
-record(cs_tasklink_get_reward_log,{
	}).
-record(p_reward_log,{
	timestamp=[] :: ?int32
	,args_name=[] :: [?string]
	,log_type=[] :: ?int32
	,reward_view=[] :: [#p_reward_view{}]}).
-record(sc_tasklink_get_reward_log,{
	reward_log_list=[] :: [#p_reward_log{}]}).
-record(cs_tasklink_sign,{
	level=[] :: ?int32}).
-record(sc_tasklink_sign,{
	result=[] :: ?int32
	,error_name=[] :: ?string}).
-record(cs_tasklink_buy_time,{
	}).
-record(sc_tasklink_buy_time,{
	result=[] :: ?int32
	,new_time=[] :: ?int32}).
-record(cs_tasklink_ready_notice,{
	}).
-record(sc_tasklink_ready_notice,{
	members=[] :: [#p_member_info{}]
	,close_timestamp=[] :: ?int32
	,diffcult_level=[] :: ?int32}).
-record(cs_tasklink_ready_opt,{
	opt_type=[] :: ?int32}).
-record(sc_tasklink_ready_opt,{
	result=[] :: ?int32}).
-record(cs_tasklink_get_reward,{
	index=[] :: ?int32}).
-record(sc_tasklink_get_reward,{
	result=[] :: ?int32
	,reward_view=[] :: [#p_reward_view{}]}).
-record(cs_activityFestival_info,{
	}).
-record(p_activityFestival_data,{
	day=[] :: ?int8
	,reward=[] :: #p_reward_info{}
	,needGold=[] :: ?int16}).
-record(p_activityFestival_box,{
	id=[] :: ?int8
	,need=[] :: ?int8
	,reward=[] :: #p_reward_info{}}).
-record(sc_activityFestival_info,{
	startTime=[] :: ?int32
	,endTime=[] :: ?int32
	,displayID=[] :: ?int16
	,activityID=[] :: ?int16
	,pos=[] :: ?int8
	,data=[] :: [#p_activityFestival_data{}]
	,box=[] :: [#p_activityFestival_box{}]}).
-record(p_activityFestival_self,{
	day=[] :: ?int8
	,isSign=[] :: ?int8}).
-record(p_activityFestival_box_get,{
	id=[] :: ?int8
	,isGet=[] :: ?int8}).
-record(cs_activityFestival_self,{
	}).
-record(sc_activityFestival_self,{
	activityID=[] :: ?int16
	,self=[] :: [#p_activityFestival_self{}]
	,box=[] :: [#p_activityFestival_box_get{}]}).
-record(cs_activityFestival_sign,{
	}).
-record(sc_activityFestival_sign,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_activityFestival_box_get,{
	id=[] :: ?int8}).
-record(sc_activityFestival_box_get,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_activityFestival_sign2,{
	day=[] :: ?int8}).
-record(sc_activityFestival_sign2,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_familycross_info,{
	}).
-record(p_familycross_enermy,{
	serverID=[] :: ?int16
	,familyName=[] :: ?string}).
-record(p_familycross_info_dtl,{
	state=[] :: ?int8
	,isSign=[] :: ?int8
	,needLevel=[] :: ?int8
	,signStartTime=[] :: ?int32
	,signEndTime=[] :: ?int32
	,fightStartTime=[] :: ?int32
	,periodEndTime=[] :: ?int32
	,enermy=[] :: [#p_familycross_enermy{}]}).
-record(sc_familycross_info,{
	result=[] :: ?int8
	,info=[] :: [#p_familycross_info_dtl{}]}).
-record(cs_familycross_sign,{
	}).
-record(sc_familycross_sign,{
	result=[] :: ?int8}).
-record(p_familycross_car,{
	driver=[] :: ?int32
	,c1=[] :: ?int32
	,c2=[] :: ?int32
	,c3=[] :: ?int32
	,type=[] :: ?int8
	,id=[] :: ?int8}).
-record(cs_familycross_displayer,{
	cars=[] :: [#p_familycross_car{}]}).
-record(sc_familycross_displayer,{
	result=[] :: ?int8}).
-record(cs_familycross_displayer_info,{
	}).
-record(sc_familycross_displayer_info,{
	cars=[] :: [#p_familycross_car{}]}).
-record(p_familycross_player_fly,{
	roleID=[] :: ?int32
	,fly=[] :: ?int8}).
-record(cs_familycross_player_fly,{
	}).
-record(sc_familycross_player_fly,{
	fly=[] :: [#p_familycross_player_fly{}]}).
-record(cs_familycross_enermy_info,{
	}).
-record(sc_familycross_enermy_info,{
	familyName1=[] :: ?string
	,familyName2=[] :: ?string
	,serverID1=[] :: ?int16
	,serverID2=[] :: ?int16}).
-record(p_familycross_war_site3,{
	city=[] :: ?int8
	,site=[] :: ?int8
	,owner=[] :: ?int8}).
-record(p_city_pos,{
	x=[] :: ?int32
	,y=[] :: ?int32}).
-record(p_site_pos,{
	city=[] :: ?int8
	,x=[] :: ?int32
	,y=[] :: ?int32}).
-record(p_born_pos,{
	city=[] :: ?int8
	,type=[] :: ?int8
	,x=[] :: ?int32
	,y=[] :: ?int32}).
-record(p_familycross_war_car,{
	id=[] :: ?int8
	,type=[] :: ?int8
	,startPos=[] :: #p_city_pos{}
	,tarCity=[] :: ?int8
	,startTime=[] :: ?int32
	,driver=[] :: ?int32
	,all=[] :: [?int32]
	,driver2=[] :: ?int32}).
-record(p_familycross_fighter,{
	typeID=[] :: ?int16
	,pos=[] :: ?int8
	,blood=[] :: ?int64
	,nowBlood=[] :: ?int64}).
-record(p_familycross_war_fly,{
	car=[] :: ?int8
	,city=[] :: ?int8
	,type=[] :: ?int8
	,flyType=[] :: ?int8
	,startPos=[] :: #p_site_pos{}
	,tarSite=[] :: ?int8
	,startTime=[] :: ?int32
	,rebornTime=[] :: ?int32
	,roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,fighters=[] :: [#p_familycross_fighter{}]
	,roleName=[] :: ?string
	,level=[] :: ?int32
	,speed=[] :: ?int16}).
-record(p_familycross_war_fly2,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32}).
-record(p_familycross_war_site,{
	id=[] :: ?int8
	,city=[] :: ?int8
	,pos=[] :: #p_site_pos{}
	,fly1=[] :: [#p_familycross_war_fly2{}]
	,fly2=[] :: [#p_familycross_war_fly2{}]
	,fly3=[] :: [#p_familycross_war_fly2{}]
	,ownType=[] :: ?int8
	,onwSec=[] :: ?int32}).
-record(p_familycross_war_city,{
	id=[] :: ?int8
	,pos=[] :: #p_city_pos{}
	,siteInfo=[] :: [#p_familycross_war_site3{}]}).
-record(p_familycross_war_car2,{
	id=[] :: ?int8
	,type=[] :: ?int8}).
-record(p_familycross_war_family_dtl,{
	familyID=[] :: ?int32
	,type=[] :: ?int8
	,familyName=[] :: ?string}).
-record(cs_familycross_war_info,{
	}).
-record(p_familycross_city_des,{
	cityID=[] :: ?int8
	,familyID=[] :: ?int16
	,serverID=[] :: ?int16
	,count=[] :: ?int16}).
-record(p_familycross_head_des,{
	familyID=[] :: ?int32
	,serverID=[] :: ?int16
	,count=[] :: ?int16
	,total=[] :: ?int32}).
-record(sc_familycross_war_info,{
	startTime=[] :: ?int32
	,interval=[] :: ?int32
	,needGas=[] :: ?int32
	,familys=[] :: [#p_familycross_war_family_dtl{}]
	,city=[] :: [#p_familycross_war_city{}]
	,cars=[] :: [#p_familycross_war_car{}]
	,bornPos=[] :: [#p_born_pos{}]
	,des=[] :: [#p_familycross_head_des{}]
	,cityDes=[] :: [#p_familycross_city_des{}]}).
-record(cs_familycross_self_info,{
	}).
-record(sc_familycross_self_info,{
	self=[] :: #p_familycross_war_fly{}}).
-record(cs_familycross_drive_car,{
	id=[] :: ?int8
	,tarCity=[] :: ?int8}).
-record(sc_familycross_drive_car,{
	result=[] :: ?int8}).
-record(cs_familycross_mov,{
	city=[] :: ?int8
	,tarSite=[] :: ?int8}).
-record(sc_familycross_mov,{
	result=[] :: ?int8}).
-record(cs_familycross_attack,{
	city=[] :: ?int8
	,site=[] :: ?int8
	,tarRoleID=[] :: ?int32
	,tarServerID=[] :: ?int32}).
-record(sc_familycross_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_familycross_city_dtl,{
	city=[] :: ?int8}).
-record(sc_familycross_city_dtl,{
	id=[] :: ?int8
	,pos=[] :: #p_city_pos{}
	,cars=[] :: [#p_familycross_war_car2{}]
	,flys=[] :: [#p_familycross_war_fly{}]
	,sites=[] :: [#p_familycross_war_site{}]
	,bornPos=[] :: [#p_born_pos{}]}).
-record(cs_familycross_site_dtl,{
	city=[] :: ?int8
	,site=[] :: ?int8}).
-record(sc_familycross_site_dtl,{
	id=[] :: ?int8
	,pos=[] :: #p_site_pos{}
	,fly=[] :: [#p_familycross_war_fly{}]
	,ownerType=[] :: ?int8
	,ownSec=[] :: ?int32}).
-record(cs_familycross_fly_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32}).
-record(p_familycross_replay_dtl,{
	isRole1Win=[] :: ?int8
	,replayUID=[] :: ?int64
	,role1Name=[] :: ?string
	,role2Name=[] :: ?string}).
-record(sc_familycross_fly_dtl,{
	fly=[] :: #p_familycross_war_fly{}
	,replay=[] :: [#p_familycross_replay_dtl{}]
	,grade=[] :: ?int16}).
-record(sc_familycross_map_update,{
	car=[] :: [#p_familycross_war_car{}]
	,site=[] :: [#p_familycross_war_site3{}]}).
-record(sc_familycross_city_update,{
	fly=[] :: [#p_familycross_war_fly{}]
	,site=[] :: [#p_familycross_war_site{}]}).
-record(sc_familycross_site_update,{
	site=[] :: [#p_familycross_war_site{}]}).
-record(sc_familycross_fly_update,{
	fly=[] :: [#p_familycross_war_fly{}]}).
-record(cs_familycross_be_driver,{
	city=[] :: ?int8
	,car=[] :: ?int8}).
-record(sc_familycross_be_driver,{
	result=[] :: ?int8}).
-record(cs_familycross_drive_car_stop,{
	id=[] :: ?int8}).
-record(sc_familycross_drive_car_stop,{
	result=[] :: ?int8}).
-record(cs_familycross_mov_stop,{
	}).
-record(sc_familycross_mov_stop,{
	result=[] :: ?int8}).
-record(cs_familycross_mov_back,{
	city=[] :: ?int8
	,carID=[] :: ?int8}).
-record(sc_familycross_mov_back,{
	result=[] :: ?int8}).
-record(cs_familycross_reborn,{
	}).
-record(sc_familycross_reborn,{
	result=[] :: ?int8}).
-record(cs_familycross_replay,{
	replayUID=[] :: ?int64}).
-record(sc_familycross_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(sc_familycross_attack_update,{
	flys=[] :: [#p_familycross_war_fly{}]}).
-record(cs_familycross_own_site,{
	city=[] :: ?int8
	,site=[] :: ?int8}).
-record(sc_familycross_own_site,{
	result=[] :: ?int8
	,ownSec=[] :: ?int32}).
-record(cs_familycross_season_rank,{
	seasonid=[] :: ?int32
	,beginpos=[] :: ?int16}).
-record(p_anubis_family,{
	familyid=[] :: ?int32
	,familyname=[] :: ?string
	,serverid=[] :: ?int16
	,leadername=[] :: ?string
	,score=[] :: ?int32
	,rank=[] :: ?int32}).
-record(sc_familycross_season_rank,{
	result=[] :: ?int8
	,seasonid=[] :: ?int32
	,beginpos=[] :: ?int16
	,isend=[] :: ?int8
	,familylist=[] :: [#p_anubis_family{}]
	,ourfamily=[] :: [#p_anubis_family{}]}).
-record(cs_familycross_seasoninfo,{
	}).
-record(sc_familycross_seasoninfo,{
	result=[] :: ?int8
	,currentseasonid=[] :: ?int32
	,existseasonid=[] :: [?int32]}).
-record(cs_familycross_family_rank,{
	seasonid=[] :: ?int32}).
-record(p_anubis_family_member,{
	roleid=[] :: ?int32
	,rolename=[] :: ?string
	,killnum=[] :: ?int32
	,resource=[] :: ?int32
	,rank=[] :: ?int16}).
-record(sc_familycross_family_rank,{
	result=[] :: ?int8
	,memberlist=[] :: [#p_anubis_family_member{}]}).
-record(sc_familycross_des_update,{
	des=[] :: [#p_familycross_head_des{}]}).
-record(p_familycross_battle_rank_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,level=[] :: ?int16
	,get=[] :: ?int16
	,score=[] :: ?int16
	,kill=[] :: ?int16
	,name=[] :: ?string
	,roleType=[] :: ?int8}).
-record(cs_familycross_battle_get_rank,{
	}).
-record(sc_familycross_battle_get_rank,{
	rank=[] :: [#p_familycross_battle_rank_dtl{}]}).
-record(sc_familycross_battle_end,{
	rank=[] :: [#p_familycross_battle_rank_dtl{}]}).
-record(cs_familycross_be_driver2,{
	city=[] :: ?int8
	,car=[] :: ?int8}).
-record(sc_familycross_be_driver2,{
	result=[] :: ?int8}).
-record(cs_familycross_self_car,{
	}).
-record(sc_familycross_self_car,{
	result=[] :: ?int8
	,car=[] :: [#p_familycross_war_car{}]
	,flys=[] :: [#p_familycross_war_fly{}]}).
-record(cs_payGuide_info,{
	}).
-record(p_payGuide_unit,{
	openState=[] :: ?int16
	,task1ID=[] :: ?int16
	,task2ID=[] :: ?int16
	,task1State=[] :: ?int8
	,task2State=[] :: ?int8
	,task2Value=[] :: ?int16
	,task1Reward=[] :: [#p_reward_info{}]
	,task2Reward=[] :: [#p_reward_info{}]}).
-record(sc_payGuide_info,{
	mainGerTypeID=[] :: ?int16
	,unit=[] :: #p_payGuide_unit{}}).
-record(cs_payGuide_get_reward,{
	taskID=[] :: ?int16}).
-record(sc_payGuide_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_payGuide_seven_info,{
	}).
-record(period_state,{
	period=[] :: ?int8
	,state=[] :: ?int8}).
-record(sc_payGuide_seven_info,{
	state=0 :: ?int8
	,begintime=0 :: ?int32
	,endtime1=0 :: ?int32
	,endtime2=0 :: ?int32
	,period=0 :: ?int8
	,periodState=[] :: [#period_state{}]
	,isFirst=0 :: ?int8}).
-record(cs_payGuide_seven_period_info,{
	period=[] :: ?int8}).
-record(sc_payGuide_seven_period_info,{
	result=[] :: ?int8
	,period=0 :: ?int8
	,tasklist=[] :: [#p_task_unit{}]}).
-record(cs_payGuide_seven_draw,{
	taskID=[] :: ?int32}).
-record(sc_payGuide_seven_draw,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(sc_payGuide_seven_task_update,{
	updateTask=[] :: [#p_task_unit{}]}).
-record(sc_payGuide_seven_period_summary,{
	periodState=[] :: [#period_state{}]}).
-record(cs_homeBoss_info,{
	roleID=[] :: ?int32}).
-record(p_homeBoss,{
	activeTime=[] :: ?int32
	,from=[] :: ?int32
	,owner=[] :: ?int32
	,isRead=[] :: ?int8
	,rewardState=[] :: ?int8
	,reward=[] :: #p_reward_info{}
	,type=[] :: ?int8
	,chapter=[] :: ?int8
	,totalBlood=[] :: ?int64
	,nowBlood=[] :: ?int64
	,ownName=[] :: ?string}).
-record(sc_homeBoss_info,{
	myBoss=[] :: #p_homeBoss{}
	,ownBoss=[] :: #p_homeBoss{}}).
-record(p_boss,{
	type=[] :: ?int8
	,totalBlood=[] :: ?int64
	,nowBlood=[] :: ?int64}).
-record(cs_homeBoss_attack,{
	roleID=[] :: ?int32
	,type=[] :: ?int8}).
-record(sc_homeBoss_attack,{
	result=[] :: ?int8
	,replay=[] :: [#sc_fight_double_request{}]
	,nowBoss=[] :: [#p_homeBoss{}]
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_homeBoss_self,{
	}).
-record(sc_homeBoss_self,{
	times=[] :: ?int8
	,n=[] :: ?int8
	,goldList=[] :: [?int16]}).
-record(cs_homeBoss_buy_times,{
	n=[] :: ?int8}).
-record(sc_homeBoss_buy_times,{
	result=[] :: ?int8
	,n=[] :: ?int8}).
-record(cs_homeBoss_get_reward,{
	}).
-record(sc_homeBoss_get_reward,{
	result=[] :: ?int8}).
-record(cs_homeBoss_read,{
	roleID=[] :: ?int32}).
-record(sc_homeBoss_read,{
	}).
-record(p_lvlSgAttr_attr,{
	gerAttack=[] :: ?int32
	,gerHpMax=[] :: ?int64
	,gerSpInit=[] :: ?int32
	,gerSpMax=[] :: ?int32
	,gerCritic=[] :: ?int32
	,gerCriticReduce=[] :: ?int32
	,gerDoom=[] :: ?int32
	,gerMiss=[] :: ?int32
	,gerAbsorb=[] :: ?int32
	,gerDamageBack=[] :: ?int32
	,gerReel=[] :: ?int32
	,gerReelReduce=[] :: ?int32
	,gerPhyDefBite=[] :: ?int32
	,gerPhyDef=[] :: ?int32
	,gerMagDefBite=[] :: ?int32
	,gerMagDef=[] :: ?int32
	,gerAttackAddtion=[] :: ?int16
	,gerHpMaxAddtion=[] :: ?int16
	,gerProMean=[] :: ?int32
	,gerProMeanAddtion=[] :: ?int32
	,gerAttckToProMean=[] :: ?int32
	,gerHpToProMean=[] :: ?int32
	,gerGskillDemageRate=[] :: ?int32
	,gerUskillDemageRate=[] :: ?int32
	,gerExtraDemageRate=[] :: ?int32
	,gerDecDemageRate=[] :: ?int32
	,gerSpeed=[] :: ?int32
	,gerSpeedAddtion=[] :: ?int32}).
-record(sc_lvlSgAttr_inc,{
	type=[] :: ?int8
	,inc=[] :: ?int16
	,attr=[] :: #p_lvlSgAttr_attr{}}).
-record(cs_galactica_info,{
	}).
-record(sc_galactica_info,{
	times=[] :: ?int16
	,cur_times=[] :: ?int16
	,buy_left=[] :: ?int16
	,plane_level=[] :: ?int16
	,golds=[] :: [?int32]
	,winGas=[] :: ?int32
	,afk_punish=[] :: ?int8
	,punish_timestamp=[] :: ?int32}).
-record(cs_galactica_sign,{
	}).
-record(sc_galactica_sign,{
	result=[] :: ?int8
	,times=[] :: ?int32}).
-record(cs_galactica_buy,{
	}).
-record(sc_galactica_buy,{
	result=[] :: ?int8
	,cur_times=[] :: ?int16
	,times=[] :: ?int16}).
-record(sc_galactica_times_update,{
	times=[] :: ?int16}).
-record(p_galactica_p_s,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(p_galactica_pos,{
	x=[] :: ?int16
	,y=[] :: ?int16}).
-record(p_galactica_mine,{
	mineID=[] :: ?int8
	,movTarget=[] :: ?int8
	,maxNum=[] :: ?int8
	,pos=[] :: #p_galactica_pos{}
	,attackers=[] :: [#p_galactica_p_s{}]
	,defenders=[] :: [#p_galactica_p_s{}]
	,value=[] :: ?int32
	,startSec=[] :: ?int32
	,attackersW=[] :: [#p_galactica_p_s{}]
	,defendersW=[] :: [#p_galactica_p_s{}]}).
-record(p_galactica_mine_s,{
	mineID=[] :: ?int8
	,movTarget=[] :: ?int8
	,pos=[] :: #p_galactica_pos{}
	,attackers=[] :: [#p_galactica_p_s{}]
	,defenders=[] :: [#p_galactica_p_s{}]
	,startSec=[] :: ?int32
	,attackersW=[] :: [#p_galactica_p_s{}]
	,defendersW=[] :: [#p_galactica_p_s{}]}).
-record(p_galactica_player,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int16
	,fly=[] :: ?int8
	,type=[] :: ?int8
	,blood=[] :: ?int8
	,mineID=[] :: ?int8
	,rebornSec=[] :: ?int32
	,name=[] :: ?string
	,fightPower=[] :: ?int64
	,roleLevel=[] :: ?int16
	,startPos=[] :: #p_galactica_pos{}
	,startSec=[] :: ?int32
	,speed=[] :: ?int16}).
-record(p_galactica_player_s,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int16
	,mineID=[] :: ?int8
	,blood=[] :: ?int8
	,startPos=[] :: #p_galactica_pos{}
	,rebornSec=[] :: ?int32
	,startSec=[] :: ?int32}).
-record(p_galactica_fairy,{
	typeID=[] :: ?int32
	,maxHp=[] :: ?int32
	,nowHp=[] :: ?int32
	,rank=[] :: ?int8}).
-record(p_galactica_home,{
	homeID=[] :: ?int8
	,type=[] :: ?int8
	,mineIDs=[] :: [?int8]
	,pos=[] :: #p_galactica_pos{}}).
-record(sc_galactica_gas,{
	attackerGas=[] :: ?int32
	,defenderGas=[] :: ?int32}).
-record(cs_galactica_war_base_info,{
	}).
-record(sc_galactica_war_base_info,{
	result=[] :: ?int8
	,endTimeStamp=0 :: ?int32
	,attackerPos=[] :: #p_galactica_pos{}
	,defenderPos=[] :: #p_galactica_pos{}
	,mines=[] :: [#p_galactica_mine{}]
	,players=[] :: [#p_galactica_player{}]
	,homes=[] :: [#p_galactica_home{}]
	,attackerGas=[] :: ?int32
	,defenderGas=[] :: ?int32}).
-record(cs_galactica_self,{
	}).
-record(sc_galactica_self,{
	self=[] :: #p_galactica_player_s{}}).
-record(cs_galactica_mov,{
	mineID=[] :: ?int8}).
-record(sc_galactica_mov,{
	result=[] :: ?int8}).
-record(cs_galactica_attack,{
	tarRoleID=[] :: ?int32
	,tarServerID=[] :: ?int32
	,mineID=[] :: ?int8}).
-record(sc_galactica_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(p_galactica_home_s,{
	homeID=[] :: ?int8
	,mineIDs=[] :: [?int8]}).
-record(sc_galactica_home_s,{
	homeInfo=[] :: [#p_galactica_home_s{}]}).
-record(sc_galactica_update,{
	player=[] :: [#p_galactica_player_s{}]
	,mine=[] :: [#p_galactica_mine_s{}]}).
-record(cs_galactica_role_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32}).
-record(p_galactica_replay_dtl,{
	isRole1Win=[] :: ?int8
	,replayUID=[] :: ?int64
	,role1Name=[] :: ?string
	,role2Name=[] :: ?string}).
-record(sc_galactica_role_dtl,{
	target=[] :: [#p_galactica_fairy{}]
	,replayDtl=[] :: [#p_galactica_replay_dtl{}]
	,grade=[] :: ?int16}).
-record(cs_galactica_replay,{
	replayUID=[] :: ?int64}).
-record(sc_galactica_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_galactica_mov_stop,{
	}).
-record(sc_galactica_mov_stop,{
	result=[] :: ?int8}).
-record(p_galactica_rank_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,level=[] :: ?int16
	,score=[] :: ?int16
	,kill=[] :: ?int16
	,name=[] :: ?string
	,type=0 :: ?int8}).
-record(p_galactica_talk,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,data=[] :: ?string
	,ext=0 :: ?int8}).
-record(cs_galactica_talk,{
	data=[] :: ?string}).
-record(cs_galactica_get_talk,{
	}).
-record(sc_galactica_get_talk,{
	data=[] :: [#p_galactica_talk{}]}).
-record(cs_galactica_get_rank,{
	}).
-record(sc_galactica_get_rank,{
	rank=[] :: [#p_galactica_rank_dtl{}]}).
-record(sc_galactica_talk,{
	data=[] :: #p_galactica_talk{}}).
-record(sc_galactica_end_war,{
	type=[] :: ?int8
	,winner=[] :: ?int8
	,data=[] :: [#p_galactica_rank_dtl{}]}).
-record(cs_galactica_reborn,{
	}).
-record(sc_galactica_reborn,{
	result=[] :: ?int8}).
-record(cs_galactica_unrequest,{
	}).
-record(sc_galactica_unrequest,{
	result=[] :: ?int8}).
-record(cs_galactica_mov_in,{
	mineID=[] :: ?int8}).
-record(sc_galactica_mov_in,{
	result=[] :: ?int8}).
-record(cs_twins_info,{
	}).
-record(p_twins_rank_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,level=[] :: ?int16
	,name=[] :: ?string
	,score=[] :: ?int64
	,type=0 :: ?int8}).
-record(p_twins_box_info,{
	id=[] :: ?int8
	,status=[] :: ?int8
	,boxLevel=[] :: ?int8}).
-record(sc_twins_info,{
	times=[] :: ?int16
	,cur_times=[] :: ?int16
	,buy_left=[] :: ?int16
	,plane_level=[] :: ?int16
	,golds=[] :: [?int32]
	,box_rank=[] :: ?int8
	,display=[] :: ?int8
	,box=[] :: [#p_twins_box_info{}]
	,rank=[] :: [#p_twins_rank_dtl{}]
	,afk_punish=[] :: ?int8
	,punish_timestamp=[] :: ?int32}).
-record(cs_twins_sign,{
	level_rank=[] :: ?int8}).
-record(sc_twins_sign,{
	result=[] :: ?int8
	,times=[] :: ?int32}).
-record(cs_twins_buy,{
	}).
-record(sc_twins_buy,{
	result=[] :: ?int8
	,cur_times=[] :: ?int16
	,times=[] :: ?int16}).
-record(sc_twins_times_update,{
	times=[] :: ?int16}).
-record(p_twins_p_s,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(p_twins_pos,{
	x=[] :: ?int16
	,y=[] :: ?int16}).
-record(p_twins_mine,{
	mineID=[] :: ?int8
	,movTarget=[] :: ?int8
	,attr=[] :: ?int8
	,state=[] :: ?int8
	,display=[] :: ?int16
	,speed=[] :: ?int16
	,startSec=[] :: ?int32
	,fullblood=[] :: ?int64
	,nowblood=[] :: ?int64
	,pos=[] :: #p_twins_pos{}
	,players=[] :: [#p_twins_p_s{}]
	,blood=[] :: ?int8}).
-record(p_twins_mine_s,{
	mineID=[] :: ?int8
	,movTarget=[] :: ?int8
	,state=[] :: ?int8
	,blood=[] :: ?int8
	,startSec=[] :: ?int32
	,nowBlood=[] :: ?int64
	,startPos=[] :: #p_twins_pos{}
	,players=[] :: [#p_twins_p_s{}]}).
-record(p_twins_player,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int16
	,fly=[] :: ?int8
	,blood=[] :: ?int8
	,mineID=[] :: ?int8
	,attr=[] :: ?int8
	,startSec=[] :: ?int32
	,fightPower=[] :: ?int64
	,score=[] :: ?int64
	,startPos=[] :: #p_twins_pos{}
	,name=[] :: ?string
	,level=[] :: ?int32
	,speed=[] :: ?int16}).
-record(p_twins_player_s,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int16
	,mineID=[] :: ?int8
	,blood=[] :: ?int8
	,startPos=[] :: #p_twins_pos{}
	,startSec=[] :: ?int32
	,score=[] :: ?int64}).
-record(p_twins_fairy,{
	typeID=[] :: ?int32
	,maxHp=[] :: ?int32
	,nowHp=[] :: ?int32
	,rank=[] :: ?int8}).
-record(p_twins_home,{
	homeID=[] :: ?int8
	,pos=[] :: #p_twins_pos{}}).
-record(cs_twins_unrequest,{
	}).
-record(sc_twins_unrequest,{
	result=[] :: ?int8}).
-record(cs_twins_war_base_info,{
	}).
-record(sc_twins_war_base_info,{
	result=[] :: ?int8
	,state=0 :: ?int8
	,endTimeStamp=0 :: ?int32
	,rebornPos=[] :: #p_twins_pos{}
	,homesPos=[] :: [#p_twins_home{}]
	,mines=[] :: [#p_twins_mine{}]
	,players=[] :: [#p_twins_player{}]}).
-record(cs_twins_self,{
	}).
-record(sc_twins_self,{
	self=[] :: #p_twins_player_s{}
	,special=[] :: ?int16}).
-record(cs_twins_mov,{
	mineID=[] :: ?int8}).
-record(sc_twins_mov,{
	result=[] :: ?int8}).
-record(cs_twins_attack,{
	mineID=[] :: ?int8}).
-record(sc_twins_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward_info{}]}).
-record(sc_twins_update,{
	player=[] :: [#p_twins_player_s{}]
	,mine=[] :: [#p_twins_mine_s{}]}).
-record(cs_twins_role_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32}).
-record(sc_twins_role_dtl,{
	target=[] :: [#p_twins_fairy{}]
	,grade=[] :: ?int16}).
-record(cs_twins_mov_stop,{
	}).
-record(sc_twins_mov_stop,{
	result=[] :: ?int8}).
-record(p_twins_talk,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,data=[] :: ?string}).
-record(cs_twins_talk,{
	data=[] :: ?string}).
-record(cs_twins_get_talk,{
	}).
-record(sc_twins_get_talk,{
	data=[] :: [#p_twins_talk{}]}).
-record(cs_twins_get_rank,{
	}).
-record(sc_twins_get_rank,{
	rank=[] :: [#p_twins_rank_dtl{}]}).
-record(sc_twins_talk,{
	data=[] :: #p_twins_talk{}}).
-record(sc_twins_end_war,{
	type=[] :: ?int8
	,level=[] :: ?int8
	,rank=[] :: [#p_twins_rank_dtl{}]}).
-record(cs_twins_open_reward_box,{
	id=[] :: ?int8}).
-record(sc_twins_open_reward_box,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_bounty_self_info,{
	}).
-record(p_bounty_unit,{
	chapterID=[] :: ?int16
	,isOpened=[] :: ?int8
	,isPassed=[] :: ?int8
	,isGetReward=[] :: ?int8}).
-record(p_bounty_data,{
	restTimes=[] :: ?int8
	,alreadyBuyTimes=[] :: ?int8
	,type=[] :: ?int8
	,chapterInfo=[] :: [#p_bounty_unit{}]}).
-record(p_bounty_chapter_blood,{
	chapterID=[] :: ?int16
	,nowBlood=[] :: ?int64
	,totalBlood=[] :: ?int64}).
-record(sc_bounty_self_info,{
	data=[] :: [#p_bounty_data{}]
	,buyGold=[] :: [?int16]
	,endTime=[] :: ?int32
	,bloodData=[] :: [#p_bounty_chapter_blood{}]}).
-record(cs_bounty_challenge,{
	chapterID=[] :: ?int16
	,type=[] :: ?int8}).
-record(sc_bounty_challenge,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,blood=[] :: #p_bounty_chapter_blood{}}).
-record(cs_bounty_buy_times,{
	type=[] :: ?int8
	,but_times=[] :: ?int8}).
-record(sc_bounty_buy_times,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,restTimes=[] :: ?int32
	,alreadyBuyTimes=[] :: ?int32}).
-record(cs_bounty_get_reward,{
	chapterID=[] :: ?int16
	,type=[] :: ?int8}).
-record(sc_bounty_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_trSpecial_info,{
	}).
-record(sc_trSpecial_info,{
	specialID=[] :: ?int16
	,trainerID=[] :: ?int32
	,cost=[] :: #p_reward_info{}}).
-record(cs_trSpecial_select,{
	specialID=[] :: ?int16}).
-record(sc_trSpecial_select,{
	result=[] :: ?int8}).
-record(cs_trSpecial_clear,{
	}).
-record(sc_trSpecial_clear,{
	result=[] :: ?int8}).
-record(sc_trSpecial_fightPower,{
	fightPower=[] :: ?int64}).
-record(cs_tvcard_info,{
	}).
-record(p_tvcard_info,{
	isTurnOver=[] :: ?int8
	,reward=[] :: #p_reward_info{}}).
-record(sc_tvcard_info,{
	activityID=[] :: ?int32
	,endTime=[] :: ?int32
	,cards=[] :: [#p_tvcard_info{}]
	,costGold=[] :: [#p_reward_info{}]}).
-record(cs_tvcard_select,{
	pos=[] :: ?int8}).
-record(sc_tvcard_select,{
	result=[] :: ?int8
	,cards=[] :: [#p_tvcard_info{}]
	,reward=[] :: #p_tvcard_info{}}).
-record(cs_tvcard_rand,{
	}).
-record(sc_tvcard_rand,{
	cards=[] :: [#p_tvcard_info{}]}).
-record(cs_magicBook_swallow_ger,{
	bookID=[] :: ?int8
	,order=[] :: ?int16
	,gerID=[] :: ?int64
	,pos=[] :: ?int8}).
-record(sc_magicBook_swallow_ger,{
	result=[] :: ?int8}).
-record(p_magicBook_summary,{
	level=[] :: ?int16
	,state=[] :: ?int8
	,percent=[] :: ?int8}).
-record(cs_magicBook_summary,{
	}).
-record(sc_magicBook_summary,{
	bookState=[] :: [#p_magicBook_summary{}]}).
-record(cs_magicBook_picture_reward,{
	bookID=[] :: ?int8
	,order=[] :: ?int8}).
-record(sc_magicBook_picture_reward,{
	result=[] :: ?int8}).
-record(cs_magicBook_book_reward,{
	bookID=[] :: ?int8}).
-record(sc_magicBook_book_reward,{
	result=[] :: ?int8}).
-record(p_magicBook_attr,{
	gerAttack=[] :: ?int32
	,gerHpMax=[] :: ?int64
	,gerSpInit=[] :: ?int32
	,gerSpMax=[] :: ?int32
	,gerCritic=[] :: ?int32
	,gerCriticReduce=[] :: ?int32
	,gerDoom=[] :: ?int32
	,gerMiss=[] :: ?int32
	,gerAbsorb=[] :: ?int32
	,gerDamageBack=[] :: ?int32
	,gerReel=[] :: ?int32
	,gerReelReduce=[] :: ?int32
	,gerPhyDefBite=[] :: ?int32
	,gerPhyDef=[] :: ?int32
	,gerMagDefBite=[] :: ?int32
	,gerMagDef=[] :: ?int32
	,gerAttackAddtion=[] :: ?int16
	,gerHpMaxAddtion=[] :: ?int16
	,gerProMean=[] :: ?int32
	,gerProMeanAddtion=[] :: ?int32
	,gerAttckToProMean=[] :: ?int32
	,gerHpToProMean=[] :: ?int32}).
-record(cs_magicBook_book_info,{
	bookID=[] :: ?int8}).
-record(sc_magicBook_book_info,{
	attr=[] :: #p_magicBook_attr{}
	,state=[] :: ?string}).
-record(sc_magicBook_book_detial,{
	attr=[] :: #p_magicBook_attr{}
	,picture=[] :: ?string
	,state=[] :: ?int8
	,percent=[] :: ?int8}).
-record(cs_luckyRoll_get_list,{
	}).
-record(p_luckyRoll_card_inner,{
	pos2=[] :: ?int8
	,id=[] :: ?int8}).
-record(p_baseBoxInfo,{
	id=[] :: ?int8
	,isOpen=[] :: ?int8
	,need=[] :: ?int32
	,rewardInfo=[] :: #p_reward_info{}}).
-record(p_lucky_role_card_p,{
	pos1=[] :: ?int8
	,pos2=[] :: ?int8}).
-record(p_luckyRoll_card_outer,{
	pos1=[] :: ?int8
	,type=[] :: ?int8
	,count=[] :: ?int32
	,cardType=[] :: ?int32}).
-record(sc_luckyRoll_get_list,{
	status=[] :: ?int8
	,outers=[] :: [#p_luckyRoll_card_outer{}]
	,inners=[] :: [#p_luckyRoll_card_inner{}]
	,pos=[] :: #p_lucky_role_card_p{}
	,freeTimes=[] :: ?int8
	,boxOpenProcess=[] :: [#p_baseBoxInfo{}]
	,stopTime=[] :: ?int32
	,endTime=[] :: ?int32
	,oneTimeNeedGold=[] :: ?int16
	,refreshNeedCoin=[] :: ?int16
	,rank=[] :: ?int32
	,mark=[] :: ?int32}).
-record(cs_luckyRoll_explore_one,{
	}).
-record(p_luckyRoll_card_oneTime,{
	openCardList=[] :: [#p_luckyRoll_card_outer{}]
	,newCardList=[] :: [#p_luckyRoll_card_outer{}]
	,pos2=[] :: ?int8}).
-record(sc_luckyRoll_explore_one,{
	type=[] :: ?int8
	,mark=[] :: ?int32
	,info=[] :: [#p_luckyRoll_card_oneTime{}]}).
-record(cs_luckyRoll_refresh,{
	}).
-record(sc_luckyRoll_refresh,{
	type=[] :: ?int8
	,outer=[] :: [#p_luckyRoll_card_outer{}]}).
-record(cs_luckyRoll_open_base_box,{
	pos=[] :: ?int8}).
-record(sc_luckyRoll_open_base_box,{
	type=[] :: ?int8
	,boxOpenProcess=[] :: [#p_baseBoxInfo{}]}).
-record(p_luckyRoll_ranker,{
	type=[] :: ?int8
	,rankNum=[] :: ?int16
	,mark=[] :: ?int32
	,roleName=[] :: ?string
	,rewardInfo=[] :: #p_reward_info{}}).
-record(cs_luckyRoll_get_rankInfo,{
	}).
-record(sc_luckyRoll_get_rankInfo,{
	type=[] :: ?int8
	,isGetRankReward=[] :: ?int8
	,selfInfo=[] :: #p_luckyRoll_ranker{}
	,rankInfoList=[] :: [#p_luckyRoll_ranker{}]}).
-record(cs_luckyRoll_get_rank_Reward,{
	}).
-record(sc_luckyRoll_get_rank_Reward,{
	type=[] :: ?int8
	,rank=[] :: ?int8
	,rewardInfo=[] :: #p_reward_info{}}).
-record(sc_luckyRoll_change_state,{
	}).
-record(cs_luckyRoll_explore_ten,{
	}).
-record(sc_luckyRoll_explore_ten,{
	type=[] :: ?int8
	,mark=[] :: ?int32
	,info=[] :: [#p_luckyRoll_card_oneTime{}]}).
-record(cs_carlos_sign,{
	}).
-record(sc_carlos_sign,{
	result=[] :: ?int8
	,times=[] :: ?int32}).
-record(cs_carlos_plane_uplevel,{
	}).
-record(sc_carlos_plane_uplevel,{
	result=[] :: ?int8
	,plane_level=[] :: ?int8}).
-record(cs_carlos_buy,{
	}).
-record(sc_carlos_buy,{
	result=[] :: ?int8
	,cur_times=[] :: ?int16
	,times=[] :: ?int16}).
-record(cs_carlos_info,{
	}).
-record(sc_carlos_info,{
	times=[] :: ?int16
	,cur_times=[] :: ?int16
	,buy_left=[] :: ?int16
	,plane_level=[] :: ?int16
	,golds=[] :: [?int32]
	,winGas=[] :: ?int32
	,afk_punish=[] :: ?int8
	,punish_timestamp=[] :: ?int32}).
-record(sc_carlos_times_update,{
	times=[] :: ?int16}).
-record(p_carlos_pos,{
	x=[] :: ?int16
	,y=[] :: ?int16}).
-record(p_carlos_mine,{
	mineID=[] :: ?int8
	,owner=[] :: ?int8
	,attackerNum=[] :: ?int8
	,defenderNum=[] :: ?int8
	,gas=[] :: ?int32
	,nowGas=[] :: ?int32
	,p1Gas=[] :: ?int32
	,p2Gas=[] :: ?int32
	,ownSec=[] :: ?int32
	,pos=[] :: #p_carlos_pos{}}).
-record(p_carlos_player,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,startPos=[] :: #p_carlos_pos{}
	,endPos=[] :: #p_carlos_pos{}
	,fly=[] :: ?int8
	,type=[] :: ?int8
	,mineID=[] :: ?int8
	,blood=[] :: ?int8
	,rebornSec=[] :: ?int32
	,name=[] :: ?string
	,startSec=[] :: ?int32
	,fightPower=[] :: ?int64
	,roleLevel=[] :: ?int16
	,speed=[] :: ?int16}).
-record(p_carlos_fairy,{
	typeID=[] :: ?int32
	,maxHp=[] :: ?int32
	,nowHp=[] :: ?int32
	,rank=[] :: ?int8}).
-record(cs_carlos_war_base_info,{
	}).
-record(sc_carlos_war_base_info,{
	result=[] :: ?int8
	,endTimeStamp=0 :: ?int32
	,attackerPos=[] :: #p_carlos_pos{}
	,defenderPos=[] :: #p_carlos_pos{}
	,mines=[] :: [#p_carlos_mine{}]
	,players=[] :: [#p_carlos_player{}]}).
-record(sc_carlos_war_update,{
	newInfo=[] :: [#p_carlos_player{}]}).
-record(cs_carlos_mine_detail,{
	mineID=[] :: ?int8}).
-record(sc_carlos_mine_detail,{
	mine=[] :: #p_carlos_mine{}
	,lastOwner=[] :: ?int8
	,attackers=[] :: [#p_carlos_player{}]
	,defenders=[] :: [#p_carlos_player{}]}).
-record(cs_carlos_self,{
	}).
-record(sc_carlos_self,{
	self=[] :: #p_carlos_player{}}).
-record(cs_carlos_mov,{
	mineID=[] :: ?int8}).
-record(sc_carlos_mov,{
	result=[] :: ?int8}).
-record(cs_carlos_attack,{
	tarRoleID=[] :: ?int32
	,tarServerID=[] :: ?int32
	,mineID=[] :: ?int8}).
-record(sc_carlos_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_carlos_ownMine,{
	mineID=[] :: ?int8}).
-record(sc_carlos_ownMine,{
	result=[] :: ?int8
	,sec=[] :: ?int32}).
-record(sc_carlos_update,{
	player=[] :: [#p_carlos_player{}]
	,mine=[] :: [#p_carlos_mine{}]}).
-record(cs_carlos_role_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32}).
-record(sc_carlos_role_dtl,{
	target=[] :: [#p_carlos_fairy{}]
	,replayDtl=[] :: [#p_carlos_replay_dtl{}]
	,grade=[] :: ?int16}).
-record(cs_carlos_replay,{
	replayUID=[] :: ?int64}).
-record(sc_carlos_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_carlos_mov_stop,{
	}).
-record(sc_carlos_mov_stop,{
	result=[] :: ?int8}).
-record(cs_carlos_talk,{
	data=[] :: ?string}).
-record(cs_carlos_get_talk,{
	}).
-record(sc_carlos_get_talk,{
	data=[] :: [#p_carlos_talk{}]}).
-record(cs_carlos_get_rank,{
	}).
-record(sc_carlos_get_rank,{
	rank=[] :: [#p_carlos_rank_dtl{}]}).
-record(sc_carlos_talk,{
	data=[] :: #p_carlos_talk{}}).
-record(sc_carlos_end_war,{
	type=[] :: ?int8
	,winner=[] :: ?int8
	,data=[] :: [#p_carlos_rank_dtl{}]}).
-record(cs_carlos_reborn,{
	}).
-record(sc_carlos_reborn,{
	result=[] :: ?int8}).
-record(cs_carlos_unrequest,{
	}).
-record(sc_carlos_unrequest,{
	result=[] :: ?int8}).
-record(cs_carlos_rank_list,{
	type=[] :: ?int8}).
-record(player_rank_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,head=[] :: ?int32
	,title=[] :: ?int8
	,level=[] :: ?int16
	,score=[] :: ?int64
	,wintime=[] :: ?int32
	,losetime=[] :: ?int32
	,equaltime=[] :: ?int32
	,rank=[] :: ?int32
	,serverID=[] :: ?int32
	,plantype=[] :: ?int8
	,vip=[] :: ?int8}).
-record(sc_carlos_rank_list,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,year=[] :: ?int32
	,season=[] :: ?int32
	,ranklist=[] :: [#player_rank_info{}]
	,rolerankinfo=[] :: #player_rank_info{}}).
-record(cs_carlos_season_info,{
	}).
-record(sc_carlos_season_info,{
	result=[] :: ?int8
	,timestamp=[] :: ?int32}).
-record(cs_carlos_plane_select,{
	type=[] :: ?int8}).
-record(sc_carlos_plane_select,{
	result=[] :: ?int8}).
-record(cs_carlos_relic_sign,{
	level_rank=[] :: ?int8}).
-record(sc_carlos_relic_sign,{
	result=[] :: ?int8}).
-record(cs_carlos_relic_info,{
	}).
-record(sc_carlos_relic_info,{
	remain_times=[] :: ?int16
	,next_need=[] :: ?int32
	,remain_buy=[] :: ?int32
	,sign_state=[] :: ?int8
	,plane_level=[] :: ?int16
	,box_rank=[] :: ?int8
	,box_name=[] :: [?string]
	,box_cost=[] :: [?int32]
	,is_open=[] :: [?int8]
	,afk_punish=[] :: ?int8
	,punish_timestamp=[] :: ?int32}).
-record(cs_carlos_relic_war_base_info,{
	}).
-record(relic_role_other,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32
	,fight_power=[] :: ?int64
	,level=[] :: ?int16
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,damage_score=[] :: ?int64}).
-record(relic_island,{
	island_id=[] :: ?int32
	,island_state=[] :: ?int32
	,nature_type=[] :: ?int8
	,max_hp=[] :: ?int64
	,cur_hp=[] :: ?int64
	,boss_pos=[] :: #p_carlos_pos{}}).
-record(sc_carlos_relic_war_base_info,{
	result=[] :: ?int8
	,endTimeStamp=0 :: ?int32
	,islandes=[] :: [#relic_island{}]
	,players=[] :: [#p_carlos_player{}]
	,other_info=[] :: [#relic_role_other{}]
	,boss_active_timeout_max=[] :: ?int32
	,atk_reinforce=[] :: ?int16
	,damage_reduce=[] :: ?int16}).
-record(sc_carlos_relic_war_update,{
	players=[] :: [#p_carlos_player{}]
	,atk_reinforce=[] :: ?int16
	,damage_reduce=[] :: ?int16}).
-record(cs_carlos_relic_mov,{
	island_id=[] :: ?int32}).
-record(sc_carlos_relic_mov,{
	result=[] :: ?int8}).
-record(cs_carlos_relic_mov_stop,{
	}).
-record(sc_carlos_relic_mov_stop,{
	result=[] :: ?int8}).
-record(cs_carlos_relic_attack,{
	island_id=[] :: ?int32}).
-record(sc_carlos_relic_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,new_islang_info=[] :: #relic_island{}
	,fight_reward=[] :: #p_reward_info{}
	,is_end=[] :: ?int8}).
-record(cs_carlos_relic_active,{
	island_id=[] :: ?int32}).
-record(sc_carlos_relic_active,{
	result=[] :: ?int8
	,new_islang_info=[] :: #relic_island{}}).
-record(cs_carlos_relic_open_reward_box,{
	index=[] :: ?int8}).
-record(sc_carlos_relic_open_reward_box,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_carlos_relic_buy,{
	}).
-record(sc_carlos_relic_buy,{
	result=[] :: ?int8
	,cur_times=[] :: ?int16
	,times=[] :: ?int16
	,next_need=[] :: ?int32}).
-record(cs_carlos_relic_sign_cancel,{
	}).
-record(sc_carlos_relic_sign_cancel,{
	result=[] :: ?int8}).
-record(cs_carlos_relic_self,{
	}).
-record(sc_carlos_relic_self,{
	self=[] :: #p_carlos_player{}}).
-record(cs_carlos_relic_role_dtl,{
	roleID=[] :: ?int32
	,serverID=[] :: ?int32}).
-record(sc_carlos_relic_role_dtl,{
	target=[] :: [#p_carlos_fairy{}]
	,naturetype=[] :: ?int8
	,grade=[] :: ?int16}).
-record(sc_carlos_relic_times_update,{
	times=[] :: ?int16}).
-record(sc_carlos_relic_end_war,{
	result=[] :: ?int8
	,box_rank=[] :: ?int8
	,box_name=[] :: [?string]
	,box_cost=[] :: [?int32]
	,other_info=[] :: [#relic_role_other{}]}).
-record(cs_carlos_relic_island_detail,{
	islandID=[] :: ?int32}).
-record(sc_carlos_relic_island_detail,{
	island=[] :: #relic_island{}
	,attackers=[] :: [#p_carlos_player{}]}).
-record(cs_carlos_relic_talk,{
	data=[] :: ?string}).
-record(cs_carlos_relic_get_talk,{
	}).
-record(sc_carlos_relic_get_talk,{
	data=[] :: [#p_carlos_talk{}]}).
-record(sc_carlos_relic_talk,{
	data=[] :: #p_carlos_talk{}}).
-record(sc_carlos_relic_update,{
	players=[] :: [#p_carlos_player{}]
	,islands=[] :: [#relic_island{}]
	,boss_active_timeout=[] :: ?int32
	,atk_reinforce=[] :: ?int16
	,damage_reduce=[] :: ?int16}).
-record(cs_carlos_change_plane,{
	itemUID=[] :: ?int64}).
-record(sc_carlos_change_plane,{
	result=[] :: ?int8}).
-record(p_carlos_plane_dtl,{
	type=[] :: ?int8
	,validTime=[] :: ?int32}).
-record(sc_carlos_plane_use_info,{
	planeInfo=[] :: [#p_carlos_plane_dtl{}]
	,display=[] :: ?int8}).
-record(cs_monthVIP_info,{
	}).
-record(sc_monthVIP_info,{
	bigNeedGold=[] :: ?int16
	,littleNeedGold=[] :: ?int16
	,restBigDays=[] :: ?int32
	,restLittleDays=[] :: ?int32
	,lastBuyBigTime=[] :: ?int32
	,lastBuyLittleTime=[] :: ?int32
	,lastGetBigTime=[] :: ?int32
	,lastGetLittleTime=[] :: ?int32
	,startTime=[] :: ?int32
	,endTime=[] :: ?int32
	,onceBigDays=[] :: ?int8
	,onceLittleDays=[] :: ?int8
	,openLevel=[] :: ?int16
	,everydayBigGold=[] :: ?int16
	,everydayLittleGold=[] :: ?int16
	,bigRewardInfo=[] :: #p_reward_info{}
	,littleRewardInfo=[] :: #p_reward_info{}
	,todayPayBig=[] :: ?int8
	,todayPayLittle=[] :: ?int8}).
-record(sc_monthVip_success,{
	type=[] :: ?int8
	,days=[] :: ?int16
	,reward=[] :: #p_reward_info{}}).
-record(cs_monthVIP_get_reward,{
	type=[] :: ?int8}).
-record(sc_monthVIP_get_reward,{
	result=[] :: ?int8
	,gold=[] :: ?int16}).
-record(cs_monthVIP_buy,{
	type=[] :: ?int8}).
-record(sc_monthVIP_buy,{
	result=[] :: ?int8
	,days=[] :: ?int16
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_monthVIP_get_growth_fund_info,{
	}).
-record(p_growth_fund_state,{
	level=[] :: ?int32
	,state=[] :: ?int8
	,growth_reward=[] :: #p_reward_info{}}).
-record(sc_monthVIP_get_growth_fund_info,{
	is_buy=[] :: ?int8
	,state_list=[] :: [#p_growth_fund_state{}]}).
-record(cs_monthVIP_buy_growth_fund,{
	}).
-record(sc_monthVIP_buy_growth_fund,{
	result=[] :: ?int8}).
-record(cs_monthVIP_get_growth_reward,{
	level=[] :: ?int16}).
-record(sc_monthVIP_get_growth_reward,{
	result=[] :: ?int8}).
-record(cs_talent_get_info,{
	}).
-record(p_talent,{
	talent_id=[] :: ?int32
	,cool_down_time=[] :: ?int32}).
-record(sc_talent_get_info,{
	talent_list=[] :: [#p_talent{}]
	,remains_point=[] :: ?int16
	,unlock_level=[] :: ?int32}).
-record(cs_talent_study,{
	talent_type=[] :: ?int32}).
-record(sc_talent_study,{
	result=[] :: ?int8
	,new_talent=[] :: #p_talent{}
	,need_gold=[] :: ?int32
	,can_buy_times=[] :: ?int16}).
-record(cs_talent_undo,{
	talent_type=[] :: ?int32}).
-record(sc_talent_undo,{
	result=[] :: ?int8}).
-record(cs_talent_cooldown,{
	talent_type=[] :: ?int32}).
-record(sc_talent_cooldown,{
	result=[] :: ?int8}).
-record(cs_trumpet_message,{
	type=[] :: ?int8
	,serverID=[] :: ?int16
	,message=[] :: ?string
	,bonusP=[] :: boolean()}).
-record(sc_trumpet_message,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,bonus_id=[] :: ?int32}).
-record(sc_trumpet_message_info,{
	type=[] :: ?int8
	,serverID=[] :: ?int16
	,roleName=[] :: ?string
	,level=[] :: ?int16
	,message=[] :: ?string
	,roleTitle=[] :: ?int8
	,timeStamp=[] :: ?int64
	,roleID=[] :: ?int32
	,head=[] :: ?int32
	,is_male=[] :: boolean()
	,bonus_id=[] :: ?int32}).
-record(cs_trumpet_recent_list,{
	type=[] :: ?int8}).
-record(sc_trumpet_recent_list,{
	type=[] :: ?int8
	,list=[] :: [#sc_trumpet_message_info{}]}).
-record(cs_trumpet_get_bonus,{
	bonus_type=[] :: ?int8
	,bonus_id=[] :: ?int32}).
-record(p_get_info,{
	serverID=[] :: ?int16
	,roleID=[] :: ?int32
	,roleName=[] :: ?string
	,amount=[] :: ?int32}).
-record(p_bonus_info,{
	amount=[] :: ?int32
	,left=[] :: ?int32
	,number=[] :: ?int16
	,divided=[] :: ?int16
	,timestamp=[] :: ?int32
	,list=[] :: [#p_get_info{}]}).
-record(sc_trumpet_get_bonus_failed,{
	bonus_type=[] :: ?int8
	,bonus_id=[] :: ?int32
	,reason=[] :: ?int8
	,info=[] :: #p_bonus_info{}}).
-record(sc_trumpet_get_bonus,{
	bonus_type=[] :: ?int8
	,bonus_id=[] :: ?int32
	,amount=[] :: ?int32
	,info=[] :: #p_bonus_info{}}).
-record(cs_trumpet_redpacket_status,{
	}).
-record(sc_trumpet_redpacket_status,{
	is_open=[] :: ?int32
	,end_time=[] :: ?int32
	,get_all_num=[] :: ?int32
	,pos_index=[] :: ?int8
	,this_num=[] :: ?int32}).
-record(sc_trumpet_new_redpacket_status,{
	role_name=[] :: ?string
	,rmb_num=[] :: ?int32}).
-record(cs_trumpet_get_all_publishers,{
	}).
-record(redpacket_info,{
	redpacket_type=[] :: ?int8
	,can_get_num=[] :: ?int32
	,total_num=[] :: ?int32}).
-record(p_publisher_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,head=[] :: ?int32
	,roleTitle=[] :: ?int8
	,is_male=[] :: boolean()
	,level=[] :: ?int32
	,skinInfo=[] :: #p_skin_info{}
	,redpacket_num=[] :: ?int32
	,redpacket_list=[] :: [#redpacket_info{}]
	,serverID=[] :: ?int32
	,pay_timestamp=[] :: ?int32}).
-record(sc_trumpet_get_all_publishers,{
	publisher_list=[] :: [#p_publisher_info{}]}).
-record(sc_trumpet_notice_publisher,{
	update_type=[] :: ?int8
	,publisher=[] :: #p_publisher_info{}}).
-record(cs_trumpet_redpacket_openclose,{
	type=[] :: ?int32}).
-record(sc_trumpet_redpacket_openclose,{
	result=[] :: ?int32}).
-record(cs_trumpet_redpacket_get_reward,{
	roleID=[] :: ?int32
	,redpacket_type=[] :: ?int8}).
-record(sc_trumpet_redpacket_get_reward,{
	result=[] :: ?int32
	,get_add_num=[] :: ?int32}).
-record(cs_trumpet_get_world_publishers,{
	}).
-record(sc_trumpet_get_world_publishers,{
	server_id_start=[] :: ?int32
	,server_id_end=[] :: ?int32
	,publisher_list=[] :: [#p_publisher_info{}]}).
-record(cs_changename,{
	type=[] :: ?int8
	,name=[] :: ?string}).
-record(sc_changename,{
	type=[] :: ?int8
	,result=[] :: ?int8}).
-record(cs_changename_freetimes,{
	type=[] :: ?int8}).
-record(sc_changename_freetimes,{
	type=[] :: ?int8
	,times=[] :: ?int8
	,extra=[] :: [?int16]}).
-record(cs_familyBoss_base_info,{
	}).
-record(p_family_boss_base_info,{
	pos=[] :: ?int8
	,lock=[] :: ?int8
	,attackTimes=[] :: ?int8
	,status=[] :: ?int8
	,level=[] :: ?int16
	,baseID=[] :: ?int16
	,activeTime=[] :: ?int32
	,hp=[] :: ?int64
	,maxHp=[] :: ?int64
	,maxAttackTimes=[] :: ?int8
	,isKilled=[] :: ?int8
	,bossDuration=[] :: ?int32
	,bossDeadTime=[] :: ?int32}).
-record(sc_familyBoss_base_info,{
	result=[] :: ?int8
	,infoList=[] :: [#p_family_boss_base_info{}]}).
-record(cs_familyBoss_attack,{
	pos=[] :: ?int8}).
-record(sc_familyBoss_attack,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(cs_familyBoss_hatch_egg,{
	pos=[] :: ?int8}).
-record(sc_familyBoss_hatch_egg,{
	result=[] :: ?int8}).
-record(cs_familyBoss_feed_boss,{
	pos=[] :: ?int8}).
-record(sc_familyBoss_feed_boss,{
	result=[] :: ?int8}).
-record(cs_familyBoss_set_boss_time,{
	pos=[] :: ?int8
	,time=[] :: ?int32}).
-record(sc_familyBoss_set_boss_time,{
	result=[] :: ?int8}).
-record(sc_familyBoss_boss_be_boss,{
	pos=[] :: ?int8}).
-record(sc_familyBoss_boss_born,{
	pos=[] :: ?int8
	,hp=[] :: ?int64
	,maxHp=[] :: ?int64}).
-record(cs_familyBoss_get_rank,{
	pos=[] :: ?int8}).
-record(p_family_boss_ranker,{
	roleID=[] :: ?int32
	,rank=[] :: ?int8
	,harm=[] :: ?int64
	,roleName=[] :: ?string}).
-record(sc_familyBoss_get_rank,{
	result=[] :: ?int8
	,pos=[] :: ?int8
	,rankList=[] :: [#p_family_boss_ranker{}]}).
-record(sc_familyBoss_bc_attack,{
	roleName=[] :: ?string
	,harm=[] :: ?int64
	,pos=[] :: ?int8}).
-record(sc_familyBoss_boss_dead,{
	pos=[] :: ?int8
	,reason=[] :: ?int8}).
-record(sc_familyBoss_bc_set_boss_time,{
	pos=[] :: ?int8
	,activeTime=[] :: ?int32}).
-record(sc_familyBoss_boss_unlock,{
	pos=[] :: ?int8}).
-record(cs_familyTek_info,{
	}).
-record(p_ger_view2,{
	gerTypeID=[] :: ?int32
	,gerQuality=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerNum=[] :: ?int16}).
-record(p_reward_info2,{
	coin=[] :: ?int32
	,roleExp=[] :: ?int32
	,gerExp=[] :: ?int32
	,gold=[] :: ?int32
	,reputation=[] :: ?int32
	,itemList=[] :: [#p_item_view{}]
	,gerList=[] :: [#p_ger_view2{}]}).
-record(p_familyTekDtl,{
	tekID=[] :: ?int32
	,tekLevel=[] :: ?int16
	,tekType=[] :: ?int8
	,tekWalletinfo=[] :: #p_reward_info2{}
	,tekFinish=[] :: ?int8}).
-record(sc_familyTek_info,{
	tekList=[] :: [#p_familyTekDtl{}]}).
-record(cs_familyTek_upLevel,{
	tekID=[] :: ?int32}).
-record(sc_familyTek_upLevel,{
	result=[] :: ?int8
	,tekID=[] :: ?int32
	,level=[] :: ?int16}).
-record(cs_familyTek_cost,{
	tekID=[] :: ?int32
	,tekLevel=[] :: ?int16}).
-record(sc_familyTek_cost,{
	tekID=[] :: ?int32
	,tekLevel=[] :: ?int16
	,tekCost=[] :: #p_reward_view{}}).
-record(cs_familyTek_wallet,{
	tekID=[] :: ?int32}).
-record(sc_familyTek_wallet,{
	wallet=[] :: #p_reward_info2{}}).
-record(p_familyTek_Ger_attr,{
	gerAttack=[] :: ?int32
	,gerHpMax=[] :: ?int64
	,gerSpInit=[] :: ?int32
	,gerSpMax=[] :: ?int32
	,gerCritic=[] :: ?int32
	,gerCriticReduce=[] :: ?int32
	,gerDoom=[] :: ?int32
	,gerMiss=[] :: ?int32
	,gerAbsorb=[] :: ?int32
	,gerDamageBack=[] :: ?int32
	,gerReel=[] :: ?int32
	,gerReelReduce=[] :: ?int32
	,gerPhyDefBite=[] :: ?int32
	,gerPhyDef=[] :: ?int32
	,gerMagDefBite=[] :: ?int32
	,gerMagDef=[] :: ?int32
	,gerAttackAddtion=[] :: ?int32
	,gerHpMaxAddtion=[] :: ?int16}).
-record(p_familyTek_Generate_buff,{
	roleReputationGenerateRate=[] :: ?int32
	,roleCoinSeedGenerateRate=[] :: ?int32
	,roleAdvantureTimeRecoveryRate=[] :: ?int32
	,roleEnergyRecoveryRate=[] :: ?int32
	,roleCompationRecoveryRate=[] :: ?int32
	,roleStoneRecoveryRate=[] :: ?int32
	,roleAdvantureGenerateCount=[] :: ?int32
	,role3v3CoinGenerateCount=[] :: ?int32
	,roleBattleGenerateCount=[] :: ?int32
	,roleReputationGenerateCount=[] :: ?int32
	,roleCoinSeedGenerateCount=[] :: ?int32}).
-record(cs_familyTek_donate,{
	itemtype=[] :: ?int8
	,itemTypeID=[] :: ?int32
	,itemNum=[] :: ?int32
	,tekID=[] :: ?int32
	,tekLevel=[] :: ?int16}).
-record(sc_familyTek_donate,{
	result=[] :: ?int8
	,itemTypeID=[] :: ?int32
	,itemNum=[] :: ?int32
	,tekID=[] :: ?int32
	,tekLevel=[] :: ?int16
	,tekWallet=[] :: #p_reward_info2{}}).
-record(sc_family_levelup,{
	familyLevel=[] :: ?int16}).
-record(cs_familyfight_info,{
	}).
-record(p_familyfight_info_dtl,{
	state=[] :: ?int8
	,isSign=[] :: ?int8
	,signNeedNum=[] :: ?int16
	,period=[] :: ?int16
	,startTime=[] :: ?int32
	,prepareEndTime=[] :: ?int32
	,randomEndTime=[] :: ?int32
	,fightEndTime=[] :: ?int32
	,periodEndTime=[] :: ?int32
	,scoreCount=[] :: ?int32
	,getStars=[] :: ?int16
	,attackTimes=[] :: ?int16
	,attackTimesUsed=[] :: ?int16
	,defendTimes=[] :: ?int16
	,defendTimesUsed=[] :: ?int16
	,worldRank=[] :: ?int32
	,winScore=[] :: ?int16
	,eqScore=[] :: ?int16
	,loseScore=[] :: ?int16}).
-record(sc_familyfight_info,{
	result=[] :: ?int8
	,info=[] :: [#p_familyfight_info_dtl{}]}).
-record(cs_familyfight_sign,{
	roleID_list=[] :: [?int32]}).
-record(sc_familyfight_sign,{
	result=[] :: ?int8}).
-record(p_familyfighter_member_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,rolefamilyTitle=[] :: ?int8
	,isJoinWar=[] :: ?int8
	,attackTimes=[] :: ?int8
	,defendTimes=[] :: ?int8
	,winStar=[] :: ?int8
	,head=[] :: ?int32
	,title=[] :: ?int8
	,is_male=[] :: boolean()
	,roleLevel=[] :: ?int16
	,fight_power=[] :: ?int64}).
-record(p_familyfighter_info_dtl,{
	family_id=[] :: ?int32
	,server_id=[] :: ?int32
	,family_name=[] :: ?string
	,winStar=[] :: ?int32
	,worldRank=[] :: ?int32
	,memberList=[] :: [#p_familyfighter_member_info{}]}).
-record(cs_familyfight_fighter_info,{
	}).
-record(sc_familyfight_fighter_info,{
	result=[] :: ?int8
	,selfFamily=[] :: [#p_familyfighter_info_dtl{}]
	,otherFamily=[] :: [#p_familyfighter_info_dtl{}]}).
-record(cs_familyfight_attack,{
	serverid=[] :: ?int32
	,familyid=[] :: ?int32
	,roleid=[] :: ?int32}).
-record(p_familyfight_record_dtl,{
	recordUID=[] :: ?int64
	,winScore=[] :: ?int8
	,fightInfo=[] :: #sc_fight_request{}
	,attackerName=[] :: ?string
	,defenderName=[] :: ?string}).
-record(sc_familyfight_attack,{
	result=[] :: ?int8
	,fight_dtl=[] :: #p_familyfight_record_dtl{}}).
-record(cs_familyfight_result,{
	}).
-record(p_familyfight_result_dtl,{
	fight_result=[] :: ?int8
	,win_star=[] :: ?int16
	,matcher_win_star=[] :: ?int16
	,now_rank=[] :: ?int32
	,old_rank=[] :: ?int32
	,matcher_server_id=[] :: ?int32
	,matcher_family_name=[] :: ?string}).
-record(sc_familyfight_result,{
	result=[] :: ?int8
	,infoList=[] :: [#p_familyfight_result_dtl{}]}).
-record(cs_familyfight_get_fight_record_list,{
	}).
-record(p_familyfight_record_info,{
	win_star=[] :: ?int8
	,result=[] :: ?int8
	,attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,winFamilyName=[] :: ?string
	,recordUID=[] :: ?int64
	,attackerRoleID=[] :: ?int32
	,defenderRoleID=[] :: ?int32}).
-record(sc_familyfight_get_fight_record_list,{
	result=[] :: ?int8
	,infoList=[] :: [#p_familyfight_record_info{}]}).
-record(cs_familyfight_replay,{
	recordUID=[] :: ?int64}).
-record(sc_familyfight_replay,{
	fightInfo=[] :: #sc_fight_request{}}).
-record(cs_familyfight_get_fighter_history,{
	tarRoleID=[] :: ?int32}).
-record(sc_familyfight_get_fighter_history,{
	historyList=[] :: [#p_familyfight_record_info{}]}).
-record(sc_familyfight_update_star_info,{
	attackRoleID=[] :: ?int32
	,defendRoleID=[] :: ?int32
	,fighterType=[] :: ?int8
	,starNum=[] :: ?int16
	,fightRecord=[] :: #p_familyfight_record_info{}}).
-record(sc_familyfight_update_state_info,{
	type=[] :: ?int8}).
-record(cs_familyfight_rankerList,{
	}).
-record(p_familyfight_ranker,{
	familyID=[] :: ?int32
	,serverID=[] :: ?int16
	,level=[] :: ?int16
	,rank=[] :: ?int32
	,familyName=[] :: ?string
	,ownerName=[] :: ?string
	,total_fight_power=[] :: ?int64
	,score=[] :: ?int16}).
-record(sc_familyfight_rankerList,{
	list=[] :: [#p_familyfight_ranker{}]}).
-record(cs_familyfight_instance_open_state,{
	}).
-record(family_instance_open_state,{
	instance_id=[] :: ?int32
	,instance_state=[] :: ?int32}).
-record(sc_familyfight_instance_open_state,{
	state_list=[] :: [#family_instance_open_state{}]
	,attack_times=[] :: ?int32
	,buy_price=[] :: ?int32
	,buy_one_times=[] :: ?int32
	,buy_price_times=[] :: ?int32
	,next_instance_id=[] :: ?int32
	,is_have_reward=[] :: ?int8}).
-record(cs_familyfight_instance_boss_info,{
	}).
-record(instance_boss_info,{
	boss_index=[] :: ?int32
	,boss_max_hp=[] :: ?int64
	,boss_cur_hp=[] :: ?int64}).
-record(sc_familyfight_instance_boss_info,{
	instance_id=[] :: ?int32
	,boss_list=[] :: [#instance_boss_info{}]}).
-record(cs_familyfight_attack_boss,{
	boss_index=[] :: ?int32}).
-record(sc_familyfight_attack_boss,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,isKillBoss=[] :: ?int8
	,fight_reward=[] :: #p_reward_info{}}).
-record(cs_familyfight_select_instance,{
	instance_id=[] :: ?int32}).
-record(sc_familyfight_select_instance,{
	result=[] :: ?int8}).
-record(cs_familyfight_instance_reward_info,{
	}).
-record(instance_damage_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,kill_num=[] :: ?int32
	,damage=[] :: ?int64}).
-record(instance_reward,{
	roleName=[] :: ?string
	,reward_index=[] :: ?int32
	,reward_info=[] :: #p_reward_info{}
	,is_open=[] :: ?int8}).
-record(sc_familyfight_instance_reward_info,{
	is_get_reward=[] :: ?int8
	,win_reward_list=[] :: [#instance_reward{}]
	,extra_reward_info=[] :: #p_reward_info{}
	,damage_list=[] :: [#instance_damage_info{}]}).
-record(cs_familyfight_instance_get_reward,{
	select_index=[] :: ?int32}).
-record(sc_familyfight_instance_get_reward,{
	result=[] :: ?int8
	,reward_detail=[] :: #instance_reward{}}).
-record(cs_familyfight_bug_attack_time,{
	times=[] :: ?int8}).
-record(sc_familyfight_bug_attack_time,{
	result=[] :: ?int8
	,new_times=[] :: ?int32}).
-record(cs_familyfight_get_fighter,{
	}).
-record(sc_familyfight_get_fighter,{
	roleID_list=[] :: [?int32]}).
-record(cs_familyfight_select_fighter,{
	roleID_list=[] :: [?int32]}).
-record(sc_familyfight_select_fighter,{
	result=[] :: ?int8}).
-record(cs_alien_info,{
	}).
-record(p_alien_fighter,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,rank=[] :: ?int32
	,serverID=[] :: ?int16
	,hpPercent=[] :: ?int8
	,canBeAtkTime=[] :: ?int32
	,killContinuousNum=[] :: ?int16
	,vip=[] :: ?int8}).
-record(sc_alien_info,{
	status=[] :: ?int8
	,endTimestamp=[] :: ?int32
	,groupID=[] :: ?int8
	,isSign=[] :: boolean()
	,leftTimes=[] :: ?int8
	,fighterList=[] :: [#p_alien_fighter{}]
	,resetTime=[] :: ?int32
	,resetNeedGold=[] :: ?int16
	,maxTimes=[] :: ?int8
	,price=[] :: ?int8
	,groupNum=[] :: ?int16}).
-record(sc_alien_sign_info,{
	needVipLevel=[] :: ?int8
	,needLevel=[] :: ?int16
	,isSign=[] :: boolean()
	,signEndTimestamp=[] :: ?int32}).
-record(cs_alien_first_five,{
	groupID=[] :: ?int16}).
-record(sc_alien_first_five,{
	fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_kill_num_rank,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_fighter2,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,rank=[] :: ?int32
	,serverID=[] :: ?int16
	,killNum=[] :: ?int16
	,vip=[] :: ?int8}).
-record(sc_alien_kill_num_rank,{
	fighterList=[] :: [#p_alien_fighter2{}]}).
-record(cs_alien_kill_continuous_rank,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_fighter3,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,rank=[] :: ?int32
	,serverID=[] :: ?int16
	,killContinuousNum=[] :: ?int16
	,isInContinuous=[] :: boolean()
	,vip=[] :: ?int8}).
-record(sc_alien_kill_continuous_rank,{
	fighterList=[] :: [#p_alien_fighter3{}]}).
-record(cs_alien_guess_info,{
	}).
-record(sc_alien_guess_info,{
	guessCoin=[] :: ?int32
	,guessType=[] :: boolean()
	,guessOddNum=[] :: ?int32
	,guessEvenNum=[] :: ?int32
	,coinValList=[] :: [?int32]}).
-record(cs_alien_guess,{
	guessCoin=[] :: ?int32
	,guessType=[] :: boolean()}).
-record(sc_alien_guess,{
	result=[] :: ?int8}).
-record(cs_alien_reset,{
	}).
-record(sc_alien_reset,{
	result=[] :: ?int8
	,timestamp=[] :: ?int32
	,fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_fight,{
	tarRoleID=[] :: ?int32
	,tarRank=[] :: ?int16}).
-record(sc_alien_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,newRank=[] :: ?int16
	,addCoin=[] :: ?int32
	,fighterList=[] :: [#p_alien_fighter{}]
	,roleconnum=[] :: ?int16
	,tarconnum=[] :: ?int16}).
-record(cs_alien_sign,{
	}).
-record(sc_alien_sign,{
	result=[] :: ?int8}).
-record(cs_alien_self_record,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_alien_self_record,{
	isAtk=[] :: boolean()
	,isWin=[] :: boolean()
	,enemyName=[] :: ?string
	,newRank=[] :: ?int16
	,replayUID=[] :: ?int64
	,timestamp=[] :: ?int32}).
-record(sc_alien_self_record,{
	recordList=[] :: [#p_alien_self_record{}]}).
-record(cs_alien_record,{
	start=[] :: ?int8
	,num=[] :: ?int8}).
-record(p_id_num,{
	typeID=[] :: ?int16
	,num=[] :: ?int32}).
-record(p_mail_reward,{
	itemList=[] :: [#p_id_num{}]
	,gerList=[] :: [#p_id_num{}]}).
-record(p_alien_record3,{
	type=[] :: ?int8
	,winIsAtk=[] :: boolean()
	,winName=[] :: ?string
	,loseName=[] :: ?string
	,continuousCount=[] :: ?int16
	,killCount=[] :: ?int16
	,replayUID=[] :: ?int64
	,timestamp=[] :: ?int32
	,reward=[] :: #p_mail_reward{}}).
-record(sc_alien_record,{
	recordList=[] :: [#p_alien_record3{}]}).
-record(p_alien_record,{
	type=[] :: ?int8
	,atkName=[] :: ?string
	,defName=[] :: ?string
	,continuousCount=[] :: ?int16
	,killCount=[] :: ?int16
	,replayUID=[] :: ?int64
	,timestamp=[] :: ?int32}).
-record(sc_alien_update_times,{
	leftTimes=[] :: ?int8
	,timestamp=[] :: ?int32}).
-record(cs_alien_self_fight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_alien_self_fight_replay,{
	fightInfoList=[] :: #sc_fight_request{}}).
-record(cs_alien_fight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_alien_fight_repaly,{
	fightInfoList=[] :: #sc_fight_request{}}).
-record(sc_alien_new_fighter_list,{
	fighterList=[] :: [#p_alien_fighter{}]}).
-record(cs_alien_leave,{
	}).
-record(sc_alien_new_self_record,{
	}).
-record(cs_alien_view_other,{
	tarRoleID=[] :: ?int32}).
-record(sc_alien_view_other,{
	tarRoleID1=[] :: ?int32
	,roleName1=[] :: ?string
	,roleLevel1=[] :: ?int16
	,fightPower1=[] :: ?int64
	,gerList1=[] :: [#p_ger{}]
	,tarRoleID2=[] :: ?int32
	,roleName2=[] :: ?string
	,roleLevel2=[] :: ?int16
	,fightPower2=[] :: ?int64
	,gerList2=[] :: [#p_ger{}]}).
-record(cs_alien_view_other_dtl,{
	tarRoleID=[] :: ?int32}).
-record(cs_alien_buy_times,{
	buyTimes=[] :: ?int8}).
-record(sc_alien_buy_times,{
	result=[] :: ?int8
	,newTimes=[] :: ?int8}).
-record(cs_alien_active,{
	}).
-record(sc_alien_active,{
	status=[] :: ?int8}).
-record(p_alien_record2,{
	type=[] :: ?int8
	,atkName=[] :: ?string
	,defName=[] :: ?string
	,continuousCount=[] :: ?int16
	,killCount=[] :: ?int16
	,replayUID=[] :: ?int64
	,timestamp=[] :: ?int32
	,reward=[] :: #p_mail_reward{}}).
-record(p_alien_finals_record,{
	atkName=[] :: ?string
	,atkID=[] :: ?int32
	,defName=[] :: ?string
	,defID=[] :: ?int32
	,isatkwin=[] :: boolean()
	,replayUID=[] :: ?int64}).
-record(p_alien_finals_round_record,{
	round=[] :: ?int8
	,records=[] :: [#p_alien_finals_record{}]}).
-record(cs_alien_finals_records,{
	type=[] :: ?int8
	,groupID=[] :: ?int8}).
-record(sc_alien_finals_records,{
	type=[] :: ?int8
	,groupID=[] :: ?int8
	,rounds=[] :: [#p_alien_finals_round_record{}]}).
-record(p_alien_finals_role_info,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,fight_power=[] :: ?int64
	,role_level=[] :: ?int16
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,vip=[] :: ?int8}).
-record(cs_alien_finals_list,{
	type=[] :: ?int8
	,groupID=[] :: ?int8}).
-record(sc_alien_finals_list,{
	type=[] :: ?int8
	,groupID=[] :: ?int8
	,list=[] :: [#p_alien_finals_role_info{}]}).
-record(cs_alien_finals_guess,{
	guessID=[] :: ?int32
	,rank=[] :: ?int8}).
-record(sc_alien_finals_guess,{
	result=[] :: ?int8}).
-record(cs_alien_finals_fight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_alien_finals_fight_replay,{
	fightInfoList=[] :: #sc_fight_request{}}).
-record(cs_alien_finals_info,{
	}).
-record(sc_alien_finals_info,{
	status=[] :: ?int8
	,round=[] :: ?int8
	,endTimestamp=[] :: ?int32}).
-record(cs_alien_finals_stake_list,{
	}).
-record(sc_alien_finals_stake_list,{
	list=[] :: [?int32]}).
-record(cs_alien_finals_self_info,{
	}).
-record(sc_alien_finals_self_info,{
	guessId=[] :: ?int32
	,guessRank=[] :: ?int8
	,groupId=[] :: ?int8}).
-record(cs_alien_self_rank,{
	}).
-record(sc_alien_self_rank,{
	isSign=[] :: boolean()
	,continuousRank=[] :: ?int32
	,killNumRank=[] :: ?int32}).
-record(cs_team_pk_info,{
	}).
-record(p_team_member,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,vip=[] :: ?int8}).
-record(sc_team_pk_open,{
	fightPower=[] :: ?int64
	,rank=[] :: ?int32
	,score=[] :: ?int32
	,refreshSelf=[] :: ?int8
	,refreshOther=[] :: ?int8
	,selfTeam=[] :: [#p_team_member{}]
	,otherTeam=[] :: [#p_team_member{}]
	,closeTimestamp=[] :: ?int32}).
-record(sc_team_pk_close,{
	fightPower=[] :: ?int64
	,score=[] :: ?int32
	,rank=[] :: ?int32
	,nextTimestamp=[] :: ?int32
	,rankList=[] :: [#p_team_member{}]}).
-record(cs_team_refresh,{
	type=[] :: ?int8}).
-record(sc_team_refresh,{
	type=[] :: ?int8
	,result=[] :: ?int8
	,list=[] :: [#p_team_member{}]}).
-record(cs_team_fight,{
	}).
-record(p_team_member2,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,vip=[] :: ?int8
	,isDead=[] :: boolean()}).
-record(sc_team_fight_result,{
	isWin=[] :: boolean()
	,addCoin=[] :: ?int32
	,addExp=[] :: ?int32
	,addScore=[] :: ?int32
	,oldRank=[] :: ?int32
	,newRank=[] :: ?int32
	,refreshSelf=[] :: ?int8
	,refreshOther=[] :: ?int8
	,otherList=[] :: [#p_team_member2{}]
	,selfList=[] :: [#p_team_member2{}]
	,fightInfoList=[] :: [#sc_fight_request{}]
	,selfTeam=[] :: [#p_team_member{}]
	,otherTeam=[] :: [#p_team_member{}]}).
-record(cs_team_rank,{
	}).
-record(p_team_member3,{
	roleID=[] :: ?int32
	,fightPower=[] :: ?int64
	,isMale=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,score=[] :: ?int32
	,rank=[] :: ?int32
	,vip=[] :: ?int8}).
-record(sc_team_rank,{
	selfRank=[] :: ?int32
	,rankList=[] :: [#p_team_member3{}]}).
-record(cs_team_record,{
	}).
-record(p_team_record,{
	isWin=[] :: boolean()
	,timestamp=[] :: ?int32
	,roleName=[] :: ?string
	,godName=[] :: ?string
	,replayUIDList=[] :: [?int64]}).
-record(sc_team_record,{
	recordList=[] :: [#p_team_record{}]}).
-record(cs_team_self_record,{
	}).
-record(p_team_self_record,{
	timestamp=[] :: ?int32
	,isWin=[] :: boolean()
	,addExp=[] :: ?int32
	,addCoin=[] :: ?int32
	,addScore=[] :: ?int32
	,selfNameList=[] :: [?string]
	,otherNameList=[] :: [?string]
	,replayUIDList=[] :: [?int64]}).
-record(sc_team_self_record,{
	recordList=[] :: [#p_team_self_record{}]}).
-record(cs_team_move,{
	fromPos=[] :: ?int8
	,toPos=[] :: ?int8}).
-record(sc_team_move,{
	result=[] :: ?int8}).
-record(sc_team_pk_not_open,{
	needLevel=[] :: ?int16}).
-record(sc_team_fight_error,{
	result=[] :: ?int8}).
-record(cs_team_fight_replay,{
	replayUIDList=[] :: [?int64]}).
-record(sc_team_fight_replay,{
	result=[] :: ?int8
	,fightInfoList=[] :: [#sc_fight_request{}]
	,otherList=[] :: [#p_team_member2{}]
	,selfList=[] :: [#p_team_member2{}]}).
-record(cs_team_self_fight_replay,{
	replayUIDList=[] :: [?int64]}).
-record(sc_team_self_fight_replay,{
	result=[] :: ?int8
	,fightInfoList=[] :: [#sc_fight_request{}]
	,otherList=[] :: [#p_team_member2{}]
	,selfList=[] :: [#p_team_member2{}]}).
-record(cs_team_view_other,{
	tarRoleID=[] :: ?int32}).
-record(sc_team_view_other,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger_view{}]}).
-record(cs_team_view_other_dtl,{
	tarRoleID=[] :: ?int32}).
-record(p_lieu_view,{
	lieuGerTypeID=[] :: ?int16
	,lieuGerQuality=[] :: ?int16
	,lieuPos=[] :: ?int8
	,lieuLevel=[] :: ?int16}).
-record(p_ger_pos,{
	gerID=[] :: ?int64
	,gerPos=[] :: ?int8}).
-record(sc_team_view_other_dtl,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger{}]
	,equipList=[] :: [#p_equip{}]
	,gerPosList=[] :: [#p_ger_pos{}]
	,atkAdd=[] :: ?int16
	,hpAdd=[] :: ?int16
	,lieuViewList=[] :: [#p_lieu_view{}]
	,head=[] :: ?int32
	,title=[] :: ?int8}).
-record(cs_team_new_status,{
	}).
-record(sc_team_new_status,{
	isOpen=[] :: boolean()}).
-record(p_race_rec,{
	atk_name=[] :: ?string
	,def_name=[] :: ?string
	,replay_uid_list=[] :: [?int64]
	,atk_role_id=[] :: ?int32
	,def_role_id=[] :: ?int32
	,atk_fight_power=[] :: ?int64
	,def_fight_power=[] :: ?int64
	,round=[] :: ?int8
	,group_id=[] :: ?int8
	,atk_is_male=[] :: boolean()
	,def_is_male=[] :: boolean()
	,atk_title=[] :: ?int8
	,def_title=[] :: ?int8
	,atk_head=[] :: ?int32
	,def_head=[] :: ?int32
	,win_or_lose_list=[] :: [boolean()]}).
-record(sc_race_new_fight,{
	new_fight=[] :: #p_race_rec{}}).
-record(p_race_fighter,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,fight_power=[] :: ?int64
	,role_level=[] :: ?int16
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32}).
-record(cs_race_history,{
	round=[] :: ?int8
	,group_id=[] :: ?int8
	,start=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_race_history,{
	round=[] :: ?int8
	,group_id=[] :: ?int8
	,history_list=[] :: [#p_race_rec{}]}).
-record(cs_race_replay,{
	replay_id=[] :: ?int64}).
-record(sc_race_replay,{
	result=[] :: ?int8
	,fight_info=[] :: #sc_fight_request{}}).
-record(cs_race_fight_list,{
	group_id=[] :: ?int8}).
-record(sc_race_fight_list,{
	group_id=[] :: ?int8
	,fighter_list=[] :: [#p_race_fighter{}]}).
-record(cs_race_sign,{
	}).
-record(sc_race_sign,{
	reason_code=[] :: ?int8}).
-record(cs_race_info,{
	}).
-record(p_race_pos,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,pos=[] :: ?int8}).
-record(sc_race_info,{
	status=[] :: ?int8
	,timestamp=[] :: ?int32
	,session_id=[] :: ?int16
	,is_sign=[] :: boolean()
	,list=[] :: [#p_race_pos{}]
	,champion_name=[] :: ?string
	,champion_id=[] :: ?int32
	,self_group_id=[] :: ?int8
	,is_auto=[] :: boolean()}).
-record(cs_race_enter,{
	}).
-record(cs_race_leave,{
	}).
-record(cs_race_pos_history,{
	pos=[] :: ?int8}).
-record(sc_race_pos_history,{
	pos=[] :: ?int8
	,race_rec=[] :: #p_race_rec{}}).
-record(sc_race_new_first,{
	new_pos=[] :: #p_race_pos{}}).
-record(sc_race_new_status,{
	status=[] :: ?int8
	,timestamp=[] :: ?int32}).
-record(cs_race_is_open,{
	}).
-record(sc_race_is_open,{
	is_open=[] :: boolean()}).
-record(cs_race_auto_sign,{
	}).
-record(sc_race_auto_sign,{
	reason_code=[] :: ?int8}).
-record(cs_race_auto_unsign,{
	}).
-record(sc_race_auto_unsign,{
	reason_code=[] :: ?int8}).
-record(cs_race_self_history,{
	}).
-record(sc_race_self_history,{
	history_list=[] :: [#p_race_rec{}]}).
-record(cs_race_guess_info,{
	}).
-record(sc_race_guess_info,{
	guessCoin=[] :: ?int32
	,roleID=[] :: ?int32
	,coinValList=[] :: [?int32]}).
-record(cs_race_guess,{
	guessCoin=[] :: ?int32
	,roleID=[] :: ?int32}).
-record(sc_race_guess,{
	result=[] :: ?int8}).
-record(cs_race2_info,{
	}).
-record(arena_fighter,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32
	,power=[] :: ?int64
	,pos=[] :: ?int8
	,level=[] :: ?int32
	,skinID=[] :: ?int32
	,trSpecialID=[] :: ?int16
	,total_gamble=[] :: ?int32}).
-record(sc_race2_info,{
	race_state=[] :: ?int8
	,sign_num=[] :: [?int32]
	,self_sign_zone=[] :: ?int8
	,group_zone=[] :: ?int8
	,top_list=[] :: [#arena_fighter{}]
	,cancel_cost=[] :: ?int32}).
-record(cs_race2_sign,{
	select_zone=[] :: ?int8}).
-record(sc_race2_sign,{
	result=[] :: ?int8}).
-record(cs_race2_openclose,{
	openclose=[] :: ?int8}).
-record(sc_race2_openclose,{
	result=[] :: ?int8}).
-record(cs_race2_arena_fighter,{
	}).
-record(sc_race2_arena_fighter,{
	arena_fighter_list=[] :: [#arena_fighter{}]}).
-record(cs_race2_fight,{
	role_id=[] :: ?int32
	,pos=[] :: ?int8}).
-record(sc_race2_fight,{
	result=[] :: ?int8
	,dice_numA=[] :: ?int8
	,dice_numB=[] :: ?int8
	,fight_rec=[] :: [#sc_fight_request{}]}).
-record(cs_race2_knockout_fighter,{
	step_num=[] :: ?int32}).
-record(race2_fight_info,{
	fighter=[] :: [#arena_fighter{}]
	,fight_timestamp=[] :: ?int32
	,left_score=[] :: ?int8
	,right_score=[] :: ?int8
	,fight_rec_id=[] :: ?int32
	,winner=[] :: ?int32
	,fight_type=[] :: ?int32
	,dice_numA=[] :: ?int8
	,dice_numB=[] :: ?int8}).
-record(sc_race2_knockout_fighter,{
	fight_info_list=[] :: [#race2_fight_info{}]}).
-record(cs_race2_get_gamble,{
	}).
-record(gamble_info,{
	gamble_type=[] :: ?int32
	,role_id=[] :: ?int32
	,gold=[] :: ?int32
	,gamble_timestamp=[] :: ?int32
	,gamble_state=[] :: ?int8}).
-record(sc_race2_get_gamble,{
	gamble_info_list=[] :: [#gamble_info{}]
	,gamble_limit=[] :: [?int32]}).
-record(cs_race2_do_gamble,{
	gamble_type=[] :: ?int32
	,role_id=[] :: ?int32
	,gold=[] :: ?int32}).
-record(sc_race2_do_gamble,{
	result=[] :: ?int8}).
-record(cs_race2_final_info,{
	}).
-record(sc_race2_final_info,{
	winner_list=[] :: [#arena_fighter{}]}).
-record(cs_race2_get_fight_rec,{
	fight_rec_id=[] :: ?int32}).
-record(sc_race2_get_fight_rec,{
	fight_rec_id=[] :: ?int32
	,dice_numA=[] :: ?int8
	,dice_numB=[] :: ?int8
	,fight_rec=[] :: [#sc_fight_request{}]}).
-record(cs_race2_cancel_sign,{
	}).
-record(sc_race2_cancel_sign,{
	result=[] :: ?int8}).
-record(p_family_member_info,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,family_contribution=[] :: ?int32
	,left_family_contribution=[] :: ?int32
	,use_gold_time=[] :: ?int32
	,title=[] :: ?int8
	,is_male=[] :: boolean()
	,online=[] :: boolean()
	,role_level=[] :: ?int16
	,fight_power=[] :: ?int64
	,family_title=[] :: ?int8
	,join_time=[] :: ?int32
	,weekly_contributes=[] :: ?int32
	,worship_times=[] :: ?int8
	,head=[] :: ?int32
	,offline_time=[] :: ?int32}).
-record(p_family_info,{
	family_id=[] :: ?int32
	,family_name=[] :: ?string
	,level=[] :: ?int16
	,create_role_id=[] :: ?int32
	,create_role_name=[] :: ?string
	,owner_role_id=[] :: ?int32
	,owner_role_name=[] :: ?string
	,cur_members=[] :: ?int16
	,active_points=[] :: ?int32
	,notice=[] :: ?string
	,members=[] :: [#p_family_member_info{}]
	,rank=[] :: ?int32
	,create_time=[] :: ?int32
	,family_score=[] :: ?int32
	,world_rank=[] :: ?int32
	,slogan=[] :: ?string
	,total_fight_power=[] :: ?int64
	,cross_rank=[] :: ?int32
	,talkRoomID=[] :: ?string}).
-record(p_family_request,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,level=[] :: ?int16
	,fight_power=[] :: ?int64
	,timestamp=[] :: ?int32
	,family_id=[] :: ?int32
	,online=[] :: boolean()
	,offline_time=[] :: ?int32
	,head=[] :: ?int32
	,title=[] :: ?int8
	,is_male=[] :: boolean()}).
-record(p_family_summary,{
	family_id=[] :: ?int32
	,family_name=[] :: ?string
	,owner_role_name=[] :: ?string
	,cur_members=[] :: ?int16
	,level=[] :: ?int16
	,rank=[] :: ?int32
	,notice=[] :: ?string
	,is_request=[] :: boolean()
	,owner_role_id=[] :: ?int32
	,world_rank=[] :: ?int32
	,slogan=[] :: ?string
	,total_fight_power=[] :: ?int64
	,score=[] :: ?int32
	,cross_rank=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(cs_family_get_list,{
	type=[] :: ?int8
	,start=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_family_get_list,{
	type=[] :: ?int8
	,result=[] :: ?int8
	,family_list=[] :: [#p_family_summary{}]}).
-record(cs_family_create,{
	family_name=[] :: ?string
	,is_gold_create=[] :: boolean()}).
-record(sc_family_create,{
	result=[] :: ?int8
	,family_info=[] :: #p_family_info{}
	,timestamp=[] :: ?int32}).
-record(cs_family_request_join,{
	family_id=[] :: ?int32}).
-record(sc_family_request_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,timestamp=[] :: ?int32
	,family_id=[] :: ?int32}).
-record(cs_family_cancel_join,{
	family_id=[] :: ?int32}).
-record(sc_family_cancel_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_id=[] :: ?int32}).
-record(cs_family_agree_join,{
	role_id=[] :: ?int32}).
-record(sc_family_agree_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_refuse_join,{
	role_id=[] :: ?int32}).
-record(sc_family_refuse_join,{
	result=[] :: ?int8
	,is_self=[] :: boolean()}).
-record(cs_family_get_info,{
	}).
-record(sc_family_get_info,{
	result=[] :: ?int8
	,family_info=[] :: #p_family_info{}
	,timestamp=[] :: ?int32}).
-record(cs_family_kick,{
	kick_role_id=[] :: ?int32}).
-record(sc_family_kick,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_create_consume,{
	}).
-record(sc_family_create_consume,{
	need_coin=[] :: ?int32
	,need_gold=[] :: ?int32}).
-record(cs_family_leave,{
	}).
-record(sc_family_leave,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,family_info=[] :: #p_family_info{}}).
-record(cs_family_change_notice,{
	notice=[] :: ?string}).
-record(sc_family_change_notice,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,notice=[] :: ?string}).
-record(cs_family_request_list,{
	}).
-record(sc_family_request_list,{
	result=[] :: ?int8
	,request_list=[] :: [#p_family_request{}]}).
-record(sc_family_del_request,{
	role_id=[] :: ?int32}).
-record(cs_family_get_log_list,{
	}).
-record(p_family_log_dtl,{
	roleid=[] :: ?int32
	,rolename=[] :: ?string
	,rolehead=[] :: ?int32
	,rolelevel=[] :: ?int32
	,timeStamp=[] :: ?int32
	,type=[] :: ?int32
	,arg_1=[] :: ?string
	,title=[] :: ?int8
	,is_male=[] :: boolean()}).
-record(sc_family_get_log_list,{
	result=[] :: ?int8
	,logDataList=[] :: [#p_family_log_dtl{}]}).
-record(cs_family_get_contribute_info,{
	}).
-record(p_familyContributeLog,{
	contributerName=[] :: ?string
	,getTypeID=[] :: ?int16}).
-record(p_familyContributeType,{
	typeID=[] :: ?int16
	,needType=[] :: ?int8
	,needValue=[] :: ?int32
	,getRice=[] :: ?int32
	,getContribute=[] :: ?int32
	,getReward=[] :: #p_reward_info{}}).
-record(sc_family_get_contribute_info,{
	result=[] :: ?int8
	,lastCTDate=[] :: ?int32
	,ctTypeInfoList=[] :: [#p_familyContributeType{}]
	,ctLogList=[] :: [#p_familyContributeLog{}]}).
-record(cs_family_do_contribute,{
	typeID=[] :: ?int16}).
-record(sc_family_do_contribute,{
	result=[] :: ?int8}).
-record(sc_family_update_exp,{
	expValue=[] :: ?int32
	,level=[] :: ?int16}).
-record(cs_family_search_by_family_name,{
	searchName=[] :: ?string}).
-record(sc_family_search_by_family_name,{
	result=[] :: ?int8
	,infoList=[] :: [#p_family_summary{}]}).
-record(cs_family_change_member_power,{
	changeRoleID=[] :: ?int32
	,typeID=[] :: ?int16}).
-record(sc_family_change_member_power,{
	result=[] :: ?int8}).
-record(sc_family_update_family_info,{
	info=[] :: #p_family_info{}}).
-record(cs_family_send_role_energy,{
	tarRoleID=[] :: ?int32}).
-record(sc_family_send_role_energy,{
	result=[] :: ?int8}).
-record(cs_family_get_role_energy,{
	roleName=[] :: ?string}).
-record(sc_family_get_role_energy,{
	result=[] :: ?int8}).
-record(cs_family_get_role_send_energy_list,{
	}).
-record(sc_family_get_role_send_energy_list,{
	roleIDList=[] :: [?int32]}).
-record(cs_family_get_member_power,{
	}).
-record(sc_family_get_member_power,{
	memberPowerList=[] :: [?int8]}).
-record(cs_family_get_send_role_energy_list,{
	}).
-record(sc_family_get_send_role_energy_list,{
	roleNameList=[] :: [?string]}).
-record(sc_family_update_contribute_log,{
	ctLogList=[] :: [#p_familyContributeLog{}]}).
-record(p_family_storage,{
	itemUID=[] :: ?int64
	,itemTypeID=[] :: ?int16
	,type=[] :: ?int8
	,reqRoleIDList=[] :: [?int32]}).
-record(cs_family_storage_info,{
	}).
-record(sc_family_storage_info,{
	result=[] :: ?int8
	,level=[] :: ?int8
	,maxLen=[] :: ?int16
	,itemLen=[] :: ?int32
	,itemList=[] :: [#p_family_storage{}]}).
-record(cs_family_storage_req,{
	itemUID=[] :: ?int64}).
-record(sc_family_storage_req,{
	result=[] :: ?int8}).
-record(cs_family_storage_assign,{
	itemUID=[] :: ?int64
	,roleID=[] :: ?int32}).
-record(sc_family_storage_assign,{
	result=[] :: ?int8}).
-record(sc_family_storage_update,{
	itemUID=[] :: ?int64
	,type=[] :: ?int8
	,reqRoleIDList=[] :: [?int32]}).
-record(cs_family_owner_impeach,{
	}).
-record(sc_family_owner_impeach,{
	result=[] :: ?int8}).
-record(cs_family_wallet,{
	}).
-record(sc_family_wallet,{
	rice=[] :: ?int64}).
-record(cs_family_change_slogan,{
	slogan=[] :: ?string}).
-record(sc_family_change_slogan,{
	result=[] :: ?int8
	,is_self=[] :: boolean()
	,slogan=[] :: ?string}).
-record(cs_family_worship,{
	type=[] :: ?int8}).
-record(sc_family_worship,{
	type=[] :: ?int8
	,result=[] :: ?int8
	,task_id=[] :: ?int32}).
-record(cs_family_worship_fight,{
	}).
-record(sc_family_worship_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: ?int32}).
-record(cs_family_worship_info,{
	}).
-record(sc_family_worship_info,{
	self=[] :: ?int8
	,family=[] :: ?int8
	,reward=[] :: ?int32}).
-record(sc_family_worship_self_push,{
	times=[] :: ?int8}).
-record(sc_family_worship_refresh_fight_reward,{
	reward=[] :: ?int32}).
-record(sc_family_worship_family_push,{
	times=[] :: ?int8
	,reward=[] :: ?int32}).
-record(cs_family_worship_limit,{
	}).
-record(sc_family_worship_limit,{
	member=[] :: ?int8
	,family=[] :: ?int8
	,cost=[] :: ?int16}).
-record(cs_family_get_member_list,{
	family_id=[] :: ?int32}).
-record(sc_family_get_member_list,{
	family_id=[] :: ?int32
	,members=[] :: [#p_family_member_info{}]}).
-record(cs_family_get_donate_contribute_list,{
	roleID=[] :: ?int32}).
-record(p_ger_donate_record_unit,{
	star=[] :: ?int8
	,donateNum=[] :: ?int64}).
-record(p_item_donate_record_unit,{
	star=[] :: ?int8
	,donateNum=[] :: ?int64}).
-record(p_donate_info,{
	diamondNum=[] :: ?int64
	,coinNum=[] :: ?int64
	,reputationNum=[] :: ?int64
	,gerdonateinfo=[] :: [#p_ger_donate_record_unit{}]
	,itemdonateinfo=[] :: [#p_item_donate_record_unit{}]}).
-record(sc_family_get_donate_contribute_list,{
	result=[] :: ?int8
	,roleID=[] :: ?int32
	,donateInfo=[] :: #p_donate_info{}}).
-record(cs_family_impeach_list,{
	}).
-record(sc_family_impeach_list,{
	result=[] :: ?int8
	,impeachmemberlist=[] :: [?int32]}).
-record(cs_family_donate_contribution_summary,{
	}).
-record(p_family_memeber_donate_info_summary,{
	roleID=[] :: ?int32
	,family_title=[] :: ?int8
	,role_name=[] :: ?string
	,is_male=[] :: boolean()
	,level=[] :: ?int16
	,donate_contribution=[] :: ?int64
	,head=[] :: ?int32
	,title=[] :: ?int8
	,vip=[] :: ?int8}).
-record(sc_family_donate_contribution_summary,{
	result=[] :: ?int8
	,donate_summary=[] :: [#p_family_memeber_donate_info_summary{}]}).
-record(cs_family_invite_request,{
	roleID=[] :: ?int32}).
-record(sc_family_invite_request,{
	result=[] :: ?int8}).
-record(cs_combine_do,{
	combineType=[] :: ?int8
	,combineTypeID=[] :: ?int16
	,combineOutType=[] :: ?int8
	,uIDList=[] :: [?int64]}).
-record(sc_combine_fail,{
	result=[] :: ?int8}).
-record(p_newGer,{
	gerTypeID=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerQuality=[] :: ?int8}).
-record(sc_combine_ger,{
	newGer=[] :: [#p_newGer{}]
	,crt_num_list=[] :: [?int32]}).
-record(p_newEquip,{
	itemTypeID=[] :: ?int16
	,itemNum=[] :: ?int16
	,itemLevel=[] :: ?int16
	,itemRank=[] :: ?int8}).
-record(sc_combine_equip,{
	newEquip=[] :: [#p_newEquip{}]
	,crt_num_list=[] :: [?int32]}).
-record(cs_combine_info,{
	}).
-record(sc_combine_info,{
	stopTime=[] :: ?int32
	,content=[] :: ?string
	,gerStarList=[] :: [?int8]
	,equipStarList=[] :: [?int8]}).
-record(cs_combine_mage_info,{
	}).
-record(p_mage_config,{
	gerTypeID=[] :: ?int16
	,need_unit=[] :: #p_sell_reward_unit{}}).
-record(sc_combine_mage_info,{
	config_list=[] :: [#p_mage_config{}]}).
-record(cs_combine_do_mage,{
	gerTypeID=[] :: ?int16
	,gerUidID=[] :: ?int64}).
-record(sc_combine_do_mage,{
	result=[] :: ?int8
	,pos=[] :: ?int8
	,newGerID=[] :: ?int64
	,return_reward=[] :: #p_reward_info{}}).
-record(cs_firecracker_open,{
	}).
-record(p_discount,{
	amount=[] :: ?int32
	,discount=[] :: ?int8}).
-record(sc_firecracker_open,{
	status=[] :: ?int8
	,name=[] :: ?string
	,description=[] :: ?string
	,icon=[] :: ?string
	,startTime=[] :: ?int32
	,rewardTime=[] :: ?int32
	,closeTime=[] :: ?int32
	,total=[] :: ?int32
	,markedPrice=[] :: ?int8
	,tradedPrice=[] :: ?int8
	,count=[] :: ?int32
	,rank=[] :: ?int8
	,canReward=[] :: ?int8
	,returnGold=[] :: ?int32
	,discounts=[] :: [#p_discount{}]
	,minLevel=[] :: ?int16
	,minVip=[] :: ?int16}).
-record(sc_firecracker_info_sync,{
	total=[] :: ?int32
	,tradedPrice=[] :: ?int8
	,returnGold=[] :: ?int32}).
-record(cs_firecracker_close,{
	}).
-record(cs_firecracker_setoff,{
	type=[] :: ?int8}).
-record(sc_firecracker_setoff,{
	result=[] :: ?int8
	,count=[] :: ?int32
	,returnGold=[] :: ?int32
	,canReward=[] :: ?int8
	,rewardInfo=[] :: [#p_reward_info{}]}).
-record(cs_firecracker_rank,{
	}).
-record(p_firecracker_rank,{
	rank=[] :: ?int8
	,name=[] :: ?string
	,count=[] :: ?int32
	,rewardInfo=[] :: [#p_reward_info{}]
	,level=[] :: ?int16}).
-record(sc_firecracker_rank,{
	rankList=[] :: [#p_firecracker_rank{}]}).
-record(cs_firecracker_get_reward,{
	}).
-record(sc_firecracker_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_firecracker_rewardList,{
	}).
-record(sc_firecracker_rewardList,{
	allrewards=[] :: #p_reward_info{}}).
-record(cs_treaHouse_get_list,{
	}).
-record(p_treaHouse_card,{
	pos=[] :: ?int8
	,posType=[] :: ?int8
	,cardType=[] :: ?int8
	,count=[] :: ?int8
	,value=[] :: ?int32}).
-record(p_baseBoxOpenInfo,{
	pos=[] :: ?int8
	,isOpen=[] :: ?int8}).
-record(sc_treaHouse_get_list,{
	isOpen=[] :: ?int8
	,freeTimes=[] :: ?int8
	,boxProcess=[] :: ?int8
	,boxOpenProcess=[] :: [#p_baseBoxOpenInfo{}]
	,endTime=[] :: ?int32
	,cardList=[] :: [#p_treaHouse_card{}]
	,oneTimeNeedGold=[] :: ?int32
	,refreshNeedCoin=[] :: ?int32
	,stopTime=[] :: ?int32
	,valueinfo=[] :: ?int8}).
-record(cs_treaHouse_is_open,{
	}).
-record(sc_treaHouse_is_open,{
	type=[] :: ?int8
	,cost=[] :: ?int8
	,icon=[] :: ?int16}).
-record(cs_treaHouse_explore_one,{
	}).
-record(p_treaHouse_card_oneTime,{
	openCardList=[] :: [#p_treaHouse_card{}]
	,newCardList=[] :: [#p_treaHouse_card{}]}).
-record(sc_treaHouse_explore_one,{
	type=[] :: ?int8
	,mark=[] :: ?int32
	,boxProcess=[] :: ?int8
	,info=[] :: [#p_treaHouse_card_oneTime{}]}).
-record(cs_treaHouse_explore_ten,{
	}).
-record(sc_treaHouse_explore_ten,{
	type=[] :: ?int8
	,mark=[] :: ?int32
	,openTimes=[] :: ?int8
	,boxProcess=[] :: ?int8
	,infoList=[] :: [#p_treaHouse_card_oneTime{}]}).
-record(cs_treaHouse_refresh,{
	}).
-record(sc_treaHouse_refresh,{
	type=[] :: ?int8
	,cardList=[] :: [#p_treaHouse_card{}]}).
-record(cs_treaHouse_open_base_box,{
	pos=[] :: ?int8}).
-record(sc_treaHouse_open_base_box,{
	type=[] :: ?int8
	,boxOpenProcess=[] :: [#p_baseBoxOpenInfo{}]}).
-record(p_treaHouse_ranker,{
	type=[] :: ?int8
	,rankNum=[] :: ?int16
	,mark=[] :: ?int32
	,roleName=[] :: ?string
	,rewardInfo=[] :: #p_reward_info{}}).
-record(cs_treaHouse_get_rankInfo,{
	}).
-record(sc_treaHouse_get_rankInfo,{
	type=[] :: ?int8
	,isGetRankReward=[] :: ?int8
	,selfInfo=[] :: #p_treaHouse_ranker{}
	,rankInfoList=[] :: [#p_treaHouse_ranker{}]}).
-record(cs_treaHouse_get_rank_Reward,{
	}).
-record(sc_treaHouse_get_rank_Reward,{
	type=[] :: ?int8
	,rank=[] :: ?int8
	,rewardInfo=[] :: #p_reward_info{}}).
-record(cs_treaHouse_get_baseBoxRewardInfo,{
	}).
-record(p_treaHouse_BaseReward_Info,{
	pos=[] :: ?int8
	,needMark=[] :: ?int32
	,rewardInfo=[] :: #p_reward_info{}}).
-record(sc_treaHouse_get_baseBoxRewardInfo,{
	baseReaward_boxInfoList=[] :: [#p_treaHouse_BaseReward_Info{}]}).
-record(sc_treaHouse_change_state,{
	}).
-record(p_cross_rec,{
	atk_name=[] :: ?string
	,def_name=[] :: ?string
	,replay_uid_list=[] :: [?int64]
	,atk_role_id=[] :: ?int32
	,def_role_id=[] :: ?int32
	,atk_fight_power=[] :: ?int64
	,def_fight_power=[] :: ?int64
	,round=[] :: ?int8
	,atk_is_male=[] :: boolean()
	,def_is_male=[] :: boolean()
	,atk_title=[] :: ?int8
	,def_title=[] :: ?int8
	,atk_server_id=[] :: ?int16
	,def_server_id=[] :: ?int16
	,win_or_lose_list=[] :: [boolean()]}).
-record(sc_cross_new_fight,{
	new_fight=[] :: #p_cross_rec{}}).
-record(cs_cross_history,{
	history_type=[] :: ?int8
	,start=[] :: ?int16
	,num=[] :: ?int16}).
-record(sc_cross_history,{
	history_type=[] :: ?int8
	,history_list=[] :: [#p_cross_rec{}]}).
-record(cs_cross_replay,{
	replay_id=[] :: ?int64
	,is_pre=[] :: boolean()}).
-record(sc_cross_replay,{
	result=[] :: ?int8
	,fight_info=[] :: #sc_fight_request{}}).
-record(sc_cross_fight_list,{
	sky_name_list=[] :: [?string]
	,ground_name_list=[] :: [?string]}).
-record(cs_cross_sign,{
	}).
-record(sc_cross_sign,{
	reason_code=[] :: ?int8}).
-record(cs_cross_info,{
	channel=[] :: ?int8}).
-record(sc_cross_info,{
	status=[] :: ?int8
	,timestamp=[] :: ?int32
	,session_id=[] :: ?int16
	,is_sign=[] :: boolean()
	,list=[] :: [#p_cross_rec{}]
	,support_role_name=[] :: ?string
	,support_price=[] :: ?int32
	,support_server_id=[] :: ?int32
	,channel=[] :: ?int8
	,is_pre_over=[] :: boolean()
	,is_all_server_ok=[] :: boolean()
	,sky_name_list=[] :: [?string]
	,ground_name_list=[] :: [?string]}).
-record(cs_cross_enter,{
	channel=[] :: ?int8}).
-record(cs_cross_leave,{
	}).
-record(cs_cross_view_list,{
	list_type=[] :: ?int8}).
-record(p_view_data,{
	name=[] :: ?string
	,server_id=[] :: ?int16}).
-record(sc_cross_view_list,{
	list_type=[] :: ?int8
	,list=[] :: [#p_view_data{}]}).
-record(cs_cross_support,{
	support_role_id=[] :: ?int32
	,support_type=[] :: ?int8}).
-record(sc_cross_support,{
	reason_code=[] :: ?int8
	,support_role_id=[] :: ?int32}).
-record(cs_cross_price_list,{
	start=[] :: ?int16
	,num=[] :: ?int16
	,list_type=[] :: ?int8}).
-record(p_price_info,{
	role_id=[] :: ?int32
	,role_name=[] :: ?string
	,is_male=[] :: boolean()
	,level=[] :: ?int16
	,total_fight_power=[] :: ?int64
	,total_price=[] :: ?int64
	,title=[] :: ?int8
	,server_id=[] :: ?int16}).
-record(sc_cross_price_list,{
	price_info_list=[] :: [#p_price_info{}]
	,support_role_id=[] :: ?int32}).
-record(cs_emperor_get_open_time,{
	}).
-record(sc_emperor_get_open_time,{
	firstOpenTime=[] :: ?int64}).
-record(cs_emperor_enter,{
	}).
-record(p_emp_fighter,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleTitle=[] :: ?int8
	,roundNum=[] :: ?int8
	,isMale=[] :: ?int8}).
-record(sc_emperor_enter,{
	result=[] :: ?int8
	,session=[] :: ?int8
	,round=[] :: ?int8
	,lround=[] :: ?int8
	,isBet=[] :: ?int16
	,betMoney=[] :: ?int32
	,betRoleID=[] :: ?int32
	,emperorID=[] :: ?int32
	,fighterInfo=[] :: [#p_emp_fighter{}]
	,nextFightTime=[] :: ?int64}).
-record(sc_emperor_broadcast_fightInfo,{
	attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,isWin=[] :: ?int8
	,pos=[] :: ?int8
	,lround=[] :: ?int8
	,fightReplayUID=[] :: ?int64}).
-record(cs_emperor_replay,{
	round_id=[] :: ?int8}).
-record(p_emperor_replayInfo,{
	attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,isWin=[] :: ?int8
	,replayUID=[] :: ?int64}).
-record(sc_emperor_replay,{
	repList=[] :: [#p_emperor_replayInfo{}]}).
-record(cs_emperor_quit,{
	}).
-record(cs_emperor_get_bet_info,{
	}).
-record(p_bet,{
	roleNameA=[] :: ?string
	,roleNameB=[] :: ?string
	,roleNameBet=[] :: ?string
	,betMoney=[] :: ?int32
	,isRight=[] :: ?int8}).
-record(sc_emperor_get_bet_info,{
	result=[] :: ?int8
	,betInfo=[] :: [#p_bet{}]}).
-record(cs_emperor_role_bet,{
	pos=[] :: ?int8
	,betID=[] :: ?int8
	,betRoleID=[] :: ?int32}).
-record(sc_emperor_role_bet,{
	result=[] :: ?int8}).
-record(cs_emperor_bet_info,{
	pos=[] :: ?int8}).
-record(sc_emperor_bet_info,{
	result=[] :: ?int8
	,betID1=[] :: ?int8
	,betMoney1=[] :: ?int32
	,betID2=[] :: ?int8
	,betMoney2=[] :: ?int32
	,betID3=[] :: ?int8
	,betMoney3=[] :: ?int32
	,roleID1=[] :: ?int32
	,aBetMoney1=[] :: ?int32
	,roleID2=[] :: ?int32
	,aBetMoney2=[] :: ?int32}).
-record(cs_emperor_get_replay,{
	repUID=[] :: ?int64}).
-record(sc_emperor_get_replay,{
	fightInfo=[] :: #sc_fight_request{}}).
-record(sc_emperor_bc_fight_end,{
	}).
-record(cs_challengeGod_info,{
	}).
-record(sc_challengeGod_info,{
	freeTimes=[] :: ?int16
	,buyTimes=[] :: ?int16
	,gerPos=[] :: ?int8
	,price=[] :: ?int8}).
-record(cs_challengeGod_select_ger,{
	pos=[] :: ?int8}).
-record(sc_challengeGod_select_ger,{
	result=[] :: ?int8}).
-record(cs_challengeGod_challenge_dungeon_one,{
	dungeonID=[] :: ?int16}).
-record(p_ger_add_exp,{
	gerPos=[] :: ?int8
	,addExp=[] :: ?int32
	,isUpgraded=[] :: boolean()}).
-record(p_reward,{
	coin=[] :: ?int32
	,roleExp=[] :: ?int32
	,gerExpList=[] :: [#p_ger_add_exp{}]
	,gold=[] :: ?int32
	,itemList=[] :: [#p_item_view{}]
	,gerList=[] :: [#p_ger_view{}]
	,levelExp=0 :: ?int32}).
-record(sc_challengeGod_challenge_dungeon_one,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward{}]}).
-record(cs_challengeGod_challenge_dungeon_ten,{
	dungeonID=[] :: ?int16}).
-record(p_challengeGod_result,{
	result=[] :: ?int8
	,reward=[] :: #p_reward{}}).
-record(sc_challengeGod_challenge_ten,{
	result=[] :: ?int8
	,info=[] :: [#p_challengeGod_result{}]}).
-record(cs_talk_world,{
	channel=[] :: ?int8
	,talkMessage=[] :: ?string}).
-record(sc_talk_world,{
	result=[] :: ?int8
	,channel=[] :: ?int8}).
-record(sc_talk_world_message,{
	channel=[] :: ?int8
	,roleName=[] :: ?string
	,message=[] :: ?string
	,roleTitle=[] :: ?int8
	,timeStamp=[] :: ?int64
	,roleID=[] :: ?int32
	,familyTitle=[] :: ?int8
	,location=[] :: ?string
	,head=[] :: ?int32
	,is_male=[] :: boolean()
	,vip_level=[] :: ?int8
	,grade=[] :: ?int8
	,level=[] :: ?int32}).
-record(cs_talk_gag_one,{
	roleName=[] :: ?string}).
-record(cs_talk_ungag_one,{
	roleName=[] :: ?string}).
-record(cs_talk_get_gag_list,{
	}).
-record(gag_info,{
	roleName=[] :: ?string
	,timeStamp=[] :: [?int32]}).
-record(sc_talk_get_gag_list,{
	gag_info_list=[] :: [#gag_info{}]}).
-record(cs_talk_recent_list,{
	channel=[] :: ?int8}).
-record(sc_talk_recent_list,{
	channel=[] :: ?int8
	,list=[] :: [#sc_talk_world_message{}]}).
-record(cs_talk_send_whisper,{
	roleID=[] :: ?int32
	,talkMessage=[] :: ?string}).
-record(sc_talk_send_whisper,{
	result=[] :: ?int8}).
-record(cs_talk_get_whisper,{
	}).
-record(p_whisper_record,{
	roleID=[] :: ?int32
	,talkMessage=[] :: ?string
	,timeStamp=[] :: ?int64}).
-record(sc_talk_get_whisper,{
	record_list=[] :: [#p_whisper_record{}]}).
-record(sc_talk_whisper_notice,{
	}).
-record(sc_push_highlight_Info,{
	value=[] :: ?int8
	,type=[] :: ?int8}).
-record(cs_nanm_open,{
	}).
-record(sc_nanm_open,{
	isOpen=[] :: ?int8
	,gerTypeID=[] :: ?int32
	,maxHp=[] :: ?int64
	,bossQuality=[] :: ?int16
	,isBuffed=[] :: boolean()
	,buffNum=[] :: ?int16
	,isOfflinePlay=[] :: boolean()
	,beginTime=[] :: ?int32
	,is_get_reward=[] :: ?int8}).
-record(sc_nanm_init_state,{
	curHp=[] :: ?int64
	,curHarm=[] :: ?int64
	,curRank=[] :: ?int32
	,rebornTime=[] :: ?int32}).
-record(cs_nanm_close,{
	}).
-record(cs_nanm_buff,{
	type=[] :: ?int8}).
-record(sc_nanm_buff,{
	type=[] :: ?int8
	,result=[] :: ?int8}).
-record(cs_nanm_last_info,{
	curSavedInfoID=[] :: ?int32}).
-record(sc_nanm_last_info_ignore,{
	}).
-record(p_nanm_info,{
	roleName=[] :: ?string
	,harmValue=[] :: ?int64}).
-record(sc_nanm_last_info_win,{
	bossQuality=[] :: ?int16
	,intervalSec=[] :: ?int32
	,bossMaxHp=[] :: ?int64
	,nanmInfolist=[] :: [#p_nanm_info{}]
	,luckyRoleList=[] :: [?string]
	,gerTypeID=[] :: ?int32}).
-record(sc_nanm_last_info_fail,{
	bossQuality=[] :: ?int16
	,intervalSec=[] :: ?int32
	,gerTypeID=[] :: ?int32}).
-record(cs_nanm_cur_info,{
	}).
-record(sc_nanm_cur_info_ignore,{
	}).
-record(sc_nanm_cur_info,{
	nanmInfoList=[] :: [#p_nanm_info{}]}).
-record(sc_nanm_hp_sync,{
	bossHp=[] :: ?int64}).
-record(p_nanm_harm,{
	name=[] :: ?string
	,harm=[] :: ?int64}).
-record(sc_nanm_harm_broadcast,{
	harmList=[] :: [#p_nanm_harm{}]}).
-record(sc_nanm_buff_sync,{
	buffNum=[] :: ?int16}).
-record(p_role_stastic,{
	harm=[] :: ?int64
	,coin=[] :: ?int32
	,repu=[] :: ?int32}).
-record(sc_nanm_stop,{
	type=[] :: ?int8
	,roleSta=[] :: #p_role_stastic{}}).
-record(cs_nanm_rank_sync,{
	}).
-record(sc_nanm_rank_sync,{
	curRank=[] :: ?int16}).
-record(cs_nanm_fight,{
	}).
-record(sc_nanm_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rebornTime=[] :: ?int32
	,rewardCoin=[] :: ?int32
	,rewardReputation=[] :: ?int32}).
-record(cs_nanm_reborn,{
	}).
-record(sc_nanm_reborn,{
	result=[] :: ?int8}).
-record(cs_nanm_offline_play,{
	openFlag=[] :: boolean()}).
-record(sc_nanm_offline_play,{
	result=[] :: ?int8
	,newOpenFlag=[] :: boolean()}).
-record(cs_nanm_open_time,{
	}).
-record(sc_nanm_open_time,{
	beginTime=[] :: ?int32}).
-record(cs_nanm_reward,{
	}).
-record(sc_nanm_reward,{
	result=[] :: ?int8
	,rewardlist=[] :: [#p_reward_view{}]}).
-record(cs_version,{
	version=[] :: ?string}).
-record(sc_version,{
	result=[] :: ?int8}).
-record(cs_gift_request,{
	code=[] :: ?string}).
-record(sc_gift_request,{
	result=[] :: ?int8
	,rewardInfo=[] :: [#p_reward_info{}]}).
-record(cs_king_enter,{
	}).
-record(sc_king_enter,{
	beginTime=[] :: ?int32
	,state=[] :: ?int8
	,round=[] :: ?int8
	,score=[] :: ?int16
	,lastPeriodID=[] :: ?int32
	,lastChampion=[] :: ?string
	,myLastRank=[] :: ?int16
	,buffLevel=[] :: ?int8
	,isSigned=[] :: boolean()
	,myCurRank=[] :: ?int16}).
-record(sc_king_enter_wait_first_session,{
	firstSignTime=[] :: ?int32}).
-record(cs_king_quit,{
	}).
-record(cs_king_sign,{
	}).
-record(sc_king_sign,{
	result=[] :: ?int8}).
-record(cs_king_buff,{
	type=[] :: ?int8}).
-record(sc_king_buff,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,newBuffLevel=[] :: ?int8}).
-record(p_rec,{
	attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,type=[] :: ?int8
	,fightReplayUID=[] :: ?int64}).
-record(sc_king_new_fight,{
	newFight=[] :: #p_rec{}}).
-record(cs_king_replay,{
	fightReplayUID=[] :: ?int64}).
-record(sc_king_replay,{
	fightInfo=[] :: #sc_fight_request{}}).
-record(cs_king_history,{
	type=[] :: ?int8
	,round=[] :: ?int8
	,startPos=[] :: ?int16
	,num=[] :: ?int8}).
-record(sc_king_history,{
	fightList=[] :: [#p_rec{}]}).
-record(cs_king_rank,{
	type=[] :: ?int8
	,startPos=[] :: ?int16
	,num=[] :: ?int8}).
-record(p_king_rank,{
	totalScore=[] :: ?int32
	,scoreList=[] :: [?int16]
	,name=[] :: ?string}).
-record(sc_king_rank,{
	type=[] :: ?int8
	,rankList=[] :: [#p_king_rank{}]}).
-record(cs_box_item,{
	itemTypeID=[] :: ?int16}).
-record(sc_box_item,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]
	,itemTypeID=[] :: ?int16}).
-record(cs_box_shop,{
	tab=[] :: ?int32
	,type=[] :: ?int8}).
-record(sc_box_shop,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(p_reward_view2,{
	type=[] :: ?int8
	,typeID=[] :: ?int16
	,num=[] :: ?int16}).
-record(cs_box_shop_info,{
	}).
-record(p_shop_box_info,{
	itemTypeID=[] :: ?int16
	,valueOne=[] :: ?int32
	,valueTen=[] :: ?int32
	,isOpenActivity=[] :: ?int8
	,discount=[] :: ?int32
	,endtime=[] :: ?int32}).
-record(sc_box_shop_info,{
	info=[] :: [#p_shop_box_info{}]}).
-record(p_reward_view3,{
	type=[] :: ?int8
	,typeID=[] :: ?int16
	,num=[] :: ?int32}).
-record(cs_box_get_spirit_equip_count,{
	}).
-record(sc_box_get_spirit_equip_count,{
	count1=[] :: ?int32
	,count2=[] :: ?int32
	,needItemTypeID1=[] :: ?int32
	,needNum1=[] :: ?int32
	,needItemTypeID2=[] :: ?int32
	,needNum2=[] :: ?int32
	,count3=[] :: ?int32
	,needItemTypeID3=[] :: ?int32
	,needNum3=[] :: ?int32}).
-record(cs_box_item_multi,{
	itemTypeID=[] :: ?int16
	,itemNum=[] :: ?int8}).
-record(sc_box_item_multi,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]
	,itemTypeID=[] :: ?int16}).
-record(cs_box_free_info,{
	}).
-record(sc_box_free_info,{
	gerTime1=[] :: ?int8
	,gerTime2=[] :: ?int8
	,itemTime1=[] :: ?int8
	,itemTime2=[] :: ?int8
	,trainerTime1=[] :: ?int8
	,trainerTime2=[] :: ?int8
	,itemfreeTimeMax=[] :: ?int8
	,buyfreeTimeMax=[] :: ?int8}).
-record(cs_box_free_open,{
	type=[] :: ?int8}).
-record(sc_box_free_open,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(sc_box_mystery_notice,{
	title=[] :: ?string
	,content=[] :: ?string
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_box_shopBoxDtl,{
	}).
-record(sc_box_shopBoxDtl,{
	spiritItemTS=0 :: ?int32
	,spiritOneTS=0 :: ?int32
	,spiritTenTS=0 :: ?int32
	,itemItemTS=0 :: ?int32
	,itemOneTS=0 :: ?int32
	,itemTenTS=0 :: ?int32
	,trainerItemTS=0 :: ?int32
	,trainerOneTS=0 :: ?int32
	,trainerTenTS=0 :: ?int32}).
-record(cs_box_shop_view,{
	tab=[] :: ?int32
	,type=[] :: ?int8}).
-record(sc_box_shop_view,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_box_shop_refresh,{
	tab=[] :: ?int32
	,type=[] :: ?int8}).
-record(sc_box_shop_refresh,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]
	,ts=0 :: ?int32}).
-record(cs_box_shop_get,{
	tab=[] :: ?int32
	,type=[] :: ?int8}).
-record(sc_box_shop_get,{
	result=[] :: ?int8}).
-record(cs_box_free_get,{
	type=[] :: ?int8}).
-record(sc_box_free_get,{
	result=[] :: ?int8}).
-record(cs_box_ticket_shop,{
	type=[] :: ?int8}).
-record(sc_box_ticket_shop,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_box_ticket_info,{
	}).
-record(sc_box_ticket_info,{
	cost1=[] :: ?int16
	,cost10=[] :: ?int16}).
-record(cs_activity_get_list,{
	}).
-record(p_activity_icon,{
	activityID=[] :: ?int16
	,iconRrc=[] :: ?string
	,activityName=[] :: ?string
	,activityType=[] :: ?int16}).
-record(sc_activity_get_list,{
	iconList=[] :: [#p_activity_icon{}]}).
-record(cs_activity_info,{
	activityID=[] :: ?int16}).
-record(p_activity_draw,{
	drawID=[] :: ?int16
	,description=[] :: ?string
	,maxDrawTimes=[] :: ?int16
	,alreadyDrawTimes=[] :: ?int16
	,canDrawTimes=[] :: ?int16
	,rewardInfo=[] :: #p_reward_info{}
	,needMaterial=[] :: [#p_reward_view{}]
	,getNum=[] :: ?int8
	,discountnum=[] :: ?int8}).
-record(sc_activity_info,{
	activityID=[] :: ?int16
	,type=[] :: ?int8
	,description=[] :: ?string
	,drawList=[] :: [#p_activity_draw{}]
	,startTime=[] :: ?int32
	,stopTime=[] :: ?int32
	,typeValue=[] :: [?int32]
	,isForever=[] :: ?int8
	,isDailyRefresh=[] :: ?int8}).
-record(cs_activity_draw,{
	activityID=[] :: ?int16
	,drawID=[] :: ?int16
	,choseReward=[] :: #p_reward_info{}}).
-record(sc_activity_draw,{
	result=[] :: ?int8
	,activityID=[] :: ?int16
	,drawID=[] :: ?int16
	,alreadyDrawTimes=[] :: ?int16
	,canDrawTimes=[] :: ?int16}).
-record(sc_activity_update,{
	activityID=[] :: ?int16
	,drawID=[] :: ?int16
	,canDrawTimes=[] :: ?int16}).
-record(sc_activity_record_update,{
	activityID=[] :: ?int16
	,typeValue=[] :: [?int32]}).
-record(p_energy_activity,{
	startTime=[] :: ?int32
	,endTime=[] :: ?int32
	,energy=[] :: ?int8
	,isGet=[] :: ?int8}).
-record(cs_activity_energy,{
	}).
-record(sc_activity_energy,{
	activityList=[] :: [#p_energy_activity{}]}).
-record(cs_activity_sign_emperor_info,{
	}).
-record(sc_activity_sign_emperor_info,{
	isSign=[] :: ?int8
	,signDays=[] :: ?int8
	,isGetBox=[] :: ?int8
	,isEmperor=[] :: ?int8
	,emperorName=[] :: ?string}).
-record(cs_activity_sign_get_reward,{
	}).
-record(sc_activity_sign_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_activity_sign_up,{
	}).
-record(sc_activity_sign_up,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_activity_rebate_info,{
	}).
-record(p_rebate_info,{
	type=[] :: ?int8
	,ratio=[] :: ?int8
	,amount=[] :: ?int32}).
-record(p_rebate_list,{
	rebateID=[] :: ?int8
	,name=[] :: ?string
	,status=[] :: ?int8
	,startTime=[] :: ?int32
	,closeTime=[] :: ?int32
	,rewardTime=[] :: ?int32
	,rebateInfo=[] :: [#p_rebate_info{}]}).
-record(sc_rebate_info,{
	status=[] :: ?int8
	,name=[] :: ?string
	,description=[] :: ?string
	,icon=[] :: ?string
	,startTime=[] :: ?int32
	,closeTime=[] :: ?int32
	,rebateList=[] :: [#p_rebate_list{}]}).
-record(cs_activity_rebate_get_reward,{
	rebateID=[] :: ?int8}).
-record(p_rebate_reward,{
	coin=[] :: ?int32
	,gold=[] :: ?int32
	,reputation=[] :: ?int32}).
-record(sc_rebate_get_reward,{
	result=[] :: ?int8
	,reward=[] :: [#p_rebate_reward{}]}).
-record(sc_rebate_update,{
	}).
-record(cs_activity_levelRank_open,{
	}).
-record(levelRank_rankerInfo,{
	roleExp=[] :: ?int64
	,roleLevel=[] :: ?int16
	,rankNum=[] :: ?int8
	,roleID=[] :: ?int32
	,roleName=[] :: ?string
	,rewardInfo=[] :: #p_reward_info{}
	,is_male=[] :: boolean()
	,title=[] :: ?int8
	,head=[] :: ?int32}).
-record(sc_activity_levelRank_open,{
	stopTime=[] :: ?int32
	,endTime=[] :: ?int32
	,rankerInfoList=[] :: [#levelRank_rankerInfo{}]
	,rankLength=[] :: ?int32
	,myRank=[] :: ?int32}).
-record(cs_activity_levelRank_refresh,{
	}).
-record(sc_activity_levelRank_refresh,{
	rankerInfoList=[] :: [#levelRank_rankerInfo{}]}).
-record(cs_activity_get_payExt_info,{
	}).
-record(cs_activity_vip_shop,{
	}).
-record(p_activity_vip_shop,{
	vip=[] :: ?int8
	,sell=[] :: ?int8
	,costType=[] :: ?int8
	,total=[] :: ?int32
	,used=[] :: ?int32
	,gold=[] :: ?int32
	,now=[] :: ?int32
	,reward=[] :: #p_reward_info{}
	,boxIcon=[] :: ?int16
	,boxName=[] :: ?string}).
-record(sc_activity_vip_shop,{
	endTime=[] :: ?int32
	,shop=[] :: [#p_activity_vip_shop{}]}).
-record(cs_activity_vip_shop_buy,{
	vip=[] :: ?int8
	,sell=[] :: ?int8}).
-record(sc_activity_vip_shop_buy,{
	type=[] :: ?int8}).
-record(p_activity_pay_reward,{
	id=[] :: ?int8
	,reward=[] :: #p_reward_info{}
	,disc=[] :: ?string}).
-record(cs_activity_first_pay,{
	}).
-record(sc_activity_first_pay,{
	displayID=[] :: ?int8
	,rewardList=[] :: [#p_activity_pay_reward{}]}).
-record(sc_activity_firstPay_update,{
	newID=[] :: ?int8
	,reward=[] :: #p_reward_info{}}).
-record(cs_activity_consume_reback,{
	}).
-record(p_consume_unit,{
	unitid=0 :: ?int32
	,consumenum=0 :: ?int64
	,rebacknum=0 :: ?int64
	,pictureid=0 :: ?int32
	,description=[] :: ?string}).
-record(sc_activity_consume_reback,{
	result=[] :: ?int8
	,activitytype=0 :: ?int8
	,consumelist=[] :: [#p_consume_unit{}]
	,totalconsume=0 :: ?int64
	,totalreback=0 :: ?int64
	,rebackrate=0 :: ?int8
	,entrancepos=0 :: ?int8
	,begintime=0 :: ?int32
	,endtime=0 :: ?int32
	,rebacktype=0 :: ?int8}).
-record(cs_activity_consume_state,{
	}).
-record(sc_activity_consume_state,{
	state=0 :: ?int8}).
-record(cs_activity_energy_pac_info,{
	}).
-record(sc_activity_energy_pac_info,{
	energyPac=0 :: ?int16
	,max=0 :: ?int16}).
-record(cs_activity_energy_pac_use,{
	}).
-record(sc_activity_energy_pac_use,{
	result=[] :: ?int8}).
-record(cs_invite_info,{
	}).
-record(sc_invite_info,{
	isBindWeibo=[] :: boolean()
	,isInputInviteCode=[] :: boolean()
	,inviteNum=[] :: ?int16
	,whoInviteYou=[] :: ?string
	,getFirstPayRewardNum=[] :: ?int16}).
-record(cs_invite_bind_weibo,{
	}).
-record(sc_invite_bind_weibo,{
	result=[] :: ?int8}).
-record(cs_invite_weibo_share_levelup,{
	level=[] :: ?int16}).
-record(sc_invite_weibo_share_levelup,{
	result=[] :: ?int8}).
-record(cs_invite_input_invite_code,{
	inviteCode=[] :: ?string}).
-record(sc_invite_input_invite_code,{
	result=[] :: ?int8
	,inviterName=[] :: ?string}).
-record(cs_invite_list,{
	}).
-record(p_invite,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int16
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,isPay=[] :: boolean()}).
-record(sc_invite_list,{
	inviteList=[] :: [#p_invite{}]}).
-record(cs_friend_get_list,{
	type=[] :: ?int8}).
-record(p_friend,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int16
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,logoutTime=[] :: ?int32
	,location=[] :: ?string
	,head=0 :: ?int32
	,matingCoolSecond=0 :: ?int32
	,gerTypeID=0 :: ?int32
	,gerQuality=0 :: ?int16
	,canGive=0 :: ?int8
	,canSend=0 :: ?int8
	,sendS=0 :: ?int32
	,beginGold=0 :: ?int32
	,endGold=0 :: ?int32
	,beginBadge=0 :: ?int32
	,endBadge=0 :: ?int32
	,chapterID=0 :: ?int16}).
-record(sc_friend_get_list,{
	type=[] :: ?int8
	,roleInfoList=[] :: [#p_friend{}]
	,giveTimes=0 :: ?int8
	,allTimes=20 :: ?int8}).
-record(cs_friend_more,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_more,{
	roleInfoList=[] :: [#p_friend{}]}).
-record(cs_friend_get_add_list,{
	}).
-record(p_stranger,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int16
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,logoutTime=[] :: ?int32
	,location=[] :: ?string
	,head=0 :: ?int32
	,canAdd=0 :: ?int8}).
-record(sc_friend_get_add_list,{
	roleList=[] :: [#p_stranger{}]}).
-record(cs_friend_add,{
	roleID=[] :: ?int32}).
-record(sc_friend_add,{
	result=[] :: ?int8}).
-record(cs_friend_explore,{
	name=[] :: ?string}).
-record(sc_friend_explore,{
	roleInfoList=[] :: [#p_stranger{}]}).
-record(cs_friend_delete,{
	type=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(sc_friend_delete,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(sc_friend_notify_delete,{
	type=[] :: ?int8
	,roleID=[] :: ?int32}).
-record(sc_friend_new,{
	type=[] :: ?int8
	,newFriend=[] :: #p_friend{}}).
-record(cs_friend_send_enargy,{
	roleID=[] :: ?int32}).
-record(sc_friend_send_enargy,{
	result=1 :: ?int8
	,roleID=0 :: ?int32}).
-record(sc_friend_send_enargy_me,{
	roleID=[] :: ?int32}).
-record(cs_friend_give_enargy,{
	roleIDList=[] :: [?int32]}).
-record(sc_friend_give_enargy,{
	result=[] :: ?int8
	,roleIDList=[] :: [?int32]
	,giveTimes=0 :: ?int8}).
-record(sc_frend_give_enargy_me,{
	roleID=[] :: ?int32
	,canSend=[] :: ?int8}).
-record(cs_friend_give_all_enargy,{
	}).
-record(sc_friend_remove_request,{
	}).
-record(cs_gather_get_list,{
	type=[] :: ?int8}).
-record(sc_gather_get_list,{
	type=[] :: ?int8
	,idList=[] :: [?int32]}).
-record(sc_gather_new,{
	type=[] :: ?int8
	,newIDList=[] :: [?int32]}).
-record(cs_gather_manual_info_for_tag,{
	tag=[] :: ?int8}).
-record(p_ger_manual_unit,{
	gerTypeID=[] :: ?int32
	,gerManual=[] :: ?int8
	,tag=[] :: ?int8}).
-record(p_collect_unit,{
	taskID=[] :: ?int32
	,finish=[] :: ?int32
	,state=[] :: ?int8
	,type=[] :: ?int8
	,tag=[] :: ?int8}).
-record(sc_gather_manual_info_for_tag,{
	tag=[] :: ?int8
	,manuallist=[] :: [#p_ger_manual_unit{}]
	,collecttask=[] :: [#p_collect_unit{}]}).
-record(sc_gather_manual_update,{
	updatemanual=[] :: [#p_ger_manual_unit{}]
	,updatecollect=[] :: [#p_collect_unit{}]}).
-record(cs_gather_manual_info,{
	}).
-record(sc_gather_manual_info,{
	totalcollect=[] :: [#p_collect_unit{}]
	,tagfinish=[] :: [?int8]}).
-record(cs_gather_manual_collect_draw,{
	taskID=[] :: ?int32}).
-record(sc_gather_manual_collect_draw,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_hist_get_list,{
	type=[] :: ?int8
	,topClientUID=[] :: ?int64}).
-record(p_hist,{
	histUID=[] :: ?int64
	,histType=[] :: ?int8
	,name=[] :: ?string
	,roleID=[] :: ?int32
	,time=[] :: ?int32
	,arg=[] :: ?int16}).
-record(sc_hist_get_list,{
	type=[] :: ?int8
	,isDiscardData=[] :: boolean()
	,historyList=[] :: [#p_hist{}]
	,unreadNum=[] :: ?int16}).
-record(cs_hist_more,{
	histUID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_hist_more,{
	type=[] :: ?int8
	,historyList=[] :: [#p_hist{}]
	,unreadNum=[] :: ?int16}).
-record(cs_hist_replay,{
	histUID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_hist_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]}).
-record(sc_hist_unreadNum,{
	type=[] :: ?int8
	,unreadNum=[] :: ?int16}).
-record(cs_mail_info,{
	type=[] :: ?int8
	,clientTopMailUID=[] :: ?int64}).
-record(p_mail,{
	mailUID=[] :: ?int64
	,mailType=[] :: ?int8
	,senderID=[] :: ?int32
	,senderName=[] :: ?string
	,content=[] :: ?string
	,time=[] :: ?int32
	,mailTemplateID=[] :: ?int16
	,paramList=[] :: [any()]
	,mailReward=[] :: [#p_mail_reward{}]
	,head=[] :: ?int32
	,isMale=[] :: boolean()}).
-record(sc_mail_info,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,isFuckData=[] :: boolean()
	,mailList=[] :: [#p_mail{}]
	,newMailNum=[] :: [?int8]}).
-record(cs_mail_draw_reward,{
	mailUID=[] :: ?int64}).
-record(sc_mail_draw_reward,{
	result=[] :: ?int8}).
-record(cs_mail_delete,{
	mailUID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_mail_delete,{
	result=[] :: ?int8}).
-record(cs_mail_new,{
	targetRoleID=[] :: ?int32
	,targetRoleName=[] :: ?string
	,content=[] :: ?string}).
-record(sc_mail_new,{
	result=[] :: ?int8}).
-record(cs_mail_unread_num,{
	}).
-record(sc_mail_unread_num,{
	newMailNum=[] :: [?int8]}).
-record(cs_mail_more,{
	type=[] :: ?int8
	,startMailUID=[] :: ?int64}).
-record(sc_mail_more,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,mailList=[] :: [#p_mail{}]
	,newMailNum=[] :: [?int8]}).
-record(cs_mail_agree_friend,{
	mailUID=[] :: ?int64}).
-record(sc_mail_agree_friend,{
	result=[] :: ?int8
	,mailUID=[] :: ?int64}).
-record(cs_mail_del_spec_mail,{
	senderID=[] :: ?int32}).
-record(sc_mail_del_spec_mail,{
	mailUIDList=[] :: [?int64]}).
-record(cs_mail_invite_operate,{
	mail_uid=[] :: ?int64
	,operate_type=[] :: ?int8}).
-record(sc_mail_invite_operate,{
	result=[] :: ?int8}).
-record(cs_mail_draw_reward_all,{
	}).
-record(sc_mail_draw_reward_all,{
	reward=[] :: [#p_reward_info{}]
	,result=[] :: ?int8}).
-record(cs_mail_canvass_info,{
	}).
-record(sc_mail_canvass_info,{
	state=[] :: ?int32
	,end_time=[] :: ?int32
	,all_num=[] :: ?int32
	,current_index=[] :: ?int32
	,reward_num=[] :: ?int32}).
-record(cs_mail_get_question,{
	index=[] :: ?int32}).
-record(sc_mail_get_question,{
	index=[] :: ?int32
	,describe=[] :: ?string
	,option_list=[] :: [?string]}).
-record(cs_mail_do_select,{
	index=[] :: ?int32
	,selected_num=[] :: ?int32}).
-record(sc_mail_do_select,{
	result=[] :: ?int8
	,state=[] :: ?int32}).
-record(cs_hron_info,{
	}).
-record(sc_hron_info_wait,{
	challengeTimes=[] :: ?int8
	,bestScore=[] :: ?int16
	,star=[] :: ?int8
	,isSelect=[] :: ?int8
	,maxBuffAdd=[] :: ?int32}).
-record(sc_hron_info_stop,{
	beginTime=[] :: ?int32}).
-record(cs_hron_info_on,{
	}).
-record(sc_hron_info_on,{
	curDungeonNum=[] :: ?int16
	,attackAdd=[] :: ?int16
	,hpAdd=[] :: ?int16
	,morale=[] :: ?int32
	,dungeonID1=[] :: ?int16
	,dungeonID2=[] :: ?int16
	,dungeonID3=[] :: ?int16
	,isHaveSuccReward=[] :: ?int16
	,challengeTimes=[] :: ?int8
	,lastFightResult=[] :: ?int8
	,historybestscore=[] :: ?int16
	,maxBuffAdd=[] :: ?int32}).
-record(sc_hron_info_on_fail,{
	result=[] :: ?int8}).
-record(cs_hron_last_rank_list,{
	star=[] :: ?int8}).
-record(p_hron_role,{
	roleName=[] :: ?string
	,score=[] :: ?int16
	,roleID=[] :: ?int32}).
-record(sc_hron_last_rank_list,{
	star=[] :: ?int8
	,rankList=[] :: [#p_hron_role{}]}).
-record(cs_hron_cur_rank_list,{
	star=[] :: ?int8}).
-record(sc_hron_cur_rank_list,{
	star=[] :: ?int8
	,rankList=[] :: [#p_hron_role{}]}).
-record(cs_hron_buy,{
	type=[] :: ?int8
	,moraleNum=[] :: ?int16}).
-record(sc_hron_buy,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,morale=[] :: ?int32
	,attackAdd=[] :: ?int16
	,hpAdd=[] :: ?int16}).
-record(cs_hron_fight,{
	type=[] :: ?int8}).
-record(sc_hron_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rewardInfo=[] :: [#p_reward{}]}).
-record(sc_hron_stop,{
	bestScore=[] :: ?int16
	,rank=[] :: ?int16}).
-record(cs_hron_rank,{
	}).
-record(sc_hron_rank,{
	bestScore=[] :: ?int16
	,rank=[] :: ?int16}).
-record(cs_hron_open_time,{
	}).
-record(sc_hron_open_time,{
	beginTime=[] :: ?int32
	,star=[] :: ?int8
	,isSelect=[] :: ?int8}).
-record(cs_hron_succ_reward,{
	}).
-record(sc_hron_succ_reward,{
	result=[] :: ?int8
	,reward_list=[] :: #p_reward_info{}}).
-record(p_hron_succ_reward,{
	reward_type=[] :: ?int16
	,reward_id=[] :: ?int32
	,reward_num=[] :: ?int16
	,is_get=[] :: boolean()}).
-record(cs_hron_select,{
	star=[] :: ?int8}).
-record(sc_hron_select,{
	result=[] :: ?int8}).
-record(cs_hron_pass,{
	}).
-record(sc_hron_pass,{
	vipLevel=[] :: ?int8
	,level=[] :: ?int16}).
-record(cs_hron_reward_view,{
	star=[] :: ?int8
	,dungeonID=[] :: ?int16}).
-record(sc_hron_reward_view,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_hron_raids,{
	}).
-record(sc_hron_raids,{
	result=[] :: ?int8
	,nextdungeonID=[] :: ?int16
	,newmorale=[] :: ?int16
	,rewardInfo=[] :: [#p_reward_info{}]}).
-record(sc_hron_update_history,{
	star=[] :: ?int8
	,besthistoryscore=[] :: ?int16}).
-record(cs_hula_open,{
	}).
-record(sc_hula_open,{
	isOpen=[] :: boolean()
	,maxHp=[] :: ?int64
	,bossQuality=[] :: ?int16
	,isBuffed=[] :: boolean()
	,buffNum=[] :: ?int16
	,isOfflinePlay=[] :: boolean()
	,beginTime=[] :: ?int32}).
-record(sc_hula_init_state,{
	curHp=[] :: ?int64
	,curHarm=[] :: ?int64
	,curRank=[] :: ?int32
	,rebornTime=[] :: ?int32}).
-record(cs_hula_close,{
	}).
-record(cs_hula_buff,{
	type=[] :: ?int8}).
-record(sc_hula_buff,{
	type=[] :: ?int8
	,result=[] :: ?int8}).
-record(cs_hula_last_info,{
	curSavedInfoID=[] :: ?int32}).
-record(sc_hula_last_info_ignore,{
	}).
-record(p_hula_info,{
	roleName=[] :: ?string
	,harmValue=[] :: ?int64}).
-record(sc_hula_last_info_win,{
	bossQuality=[] :: ?int16
	,intervalSec=[] :: ?int32
	,bossMaxHp=[] :: ?int64
	,hulaInfolist=[] :: [#p_hula_info{}]
	,luckyRoleList=[] :: [?string]}).
-record(sc_hula_last_info_fail,{
	bossQuality=[] :: ?int16
	,intervalSec=[] :: ?int32}).
-record(cs_hula_cur_info,{
	}).
-record(sc_hula_cur_info_ignore,{
	}).
-record(sc_hula_cur_info,{
	hulaInfoList=[] :: [#p_hula_info{}]}).
-record(sc_hula_hp_sync,{
	bossHp=[] :: ?int64}).
-record(p_hula_harm,{
	name=[] :: ?string
	,harm=[] :: ?int64}).
-record(sc_hula_harm_broadcast,{
	harmList=[] :: [#p_hula_harm{}]}).
-record(sc_hula_buff_sync,{
	buffNum=[] :: ?int16}).
-record(sc_hula_stop,{
	type=[] :: ?int8
	,roleSta=[] :: #p_role_stastic{}}).
-record(cs_hula_rank_sync,{
	}).
-record(sc_hula_rank_sync,{
	curRank=[] :: ?int16}).
-record(cs_hula_fight,{
	}).
-record(sc_hula_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rebornTime=[] :: ?int32
	,rewardCoin=[] :: ?int32
	,rewardReputation=[] :: ?int32}).
-record(cs_hula_reborn,{
	}).
-record(sc_hula_reborn,{
	result=[] :: ?int8}).
-record(cs_hula_offline_play,{
	openFlag=[] :: boolean()}).
-record(sc_hula_offline_play,{
	result=[] :: ?int8
	,newOpenFlag=[] :: boolean()}).
-record(cs_hula_open_time,{
	}).
-record(sc_hula_open_time,{
	beginTime=[] :: ?int32}).
-record(cs_card_get_list,{
	}).
-record(p_card,{
	type=[] :: ?int8
	,value=[] :: ?int32}).
-record(p_opened_card,{
	pos=[] :: ?int8
	,type=[] :: ?int8
	,value=[] :: ?int32}).
-record(sc_card_get_list,{
	openedCardList=[] :: [#p_opened_card{}]
	,cardList=[] :: [#p_card{}]
	,draw_cost_type=[] :: ?int32
	,draw_cost_value=[] :: ?int32}).
-record(cs_card_draw,{
	pos=[] :: ?int8}).
-record(sc_card_draw,{
	result=[] :: ?int8
	,pos=[] :: ?int8
	,card=[] :: [#p_card{}]
	,draw_cost_type=[] :: ?int32
	,draw_cost_value=[] :: ?int32}).
-record(cs_card_refresh,{
	}).
-record(sc_card_refresh,{
	result=[] :: ?int8
	,cardList=[] :: [#p_card{}]
	,draw_cost_type=[] :: ?int32
	,draw_cost_value=[] :: ?int32}).
-record(cs_card_onekey,{
	}).
-record(sc_card_onekey,{
	result=[] :: ?int8
	,card=[] :: [#p_opened_card{}]}).
-record(cs_card_activity_info,{
	}).
-record(sc_card_activity_card,{
	startTime=[] :: ?int32
	,stopTime=[] :: ?int32
	,level=[] :: ?int16
	,refresh_cost_type=[] :: ?int32
	,refresh_cost_value=[] :: ?int32}).
-record(cs_daily_get_list,{
	}).
-record(p_daily,{
	type=[] :: ?int8
	,value=[] :: ?int8
	,isDrawed=[] :: boolean()}).
-record(sc_daily_get_list,{
	dailyList=[] :: [#p_daily{}]}).
-record(cs_daily_draw,{
	type=[] :: ?int8}).
-record(sc_daily_draw,{
	result=[] :: ?int8
	,newDaily=[] :: #p_daily{}}).
-record(sc_daily_family_info,{
	result=[] :: ?int8
	,familyLevel=[] :: ?int8}).
-record(cs_plunder_info,{
	}).
-record(p_stonechip,{
	targetTypeID=[] :: ?int32
	,firstNum=[] :: ?int16
	,secondNum=[] :: ?int16
	,thirdNum=[] :: ?int16
	,fourthNum=[] :: ?int16}).
-record(sc_plunder_info,{
	isOpen=[] :: boolean()
	,protectEndTime=[] :: ?int32
	,restAttackTimes=[] :: ?int16
	,maxAttackTimes=[] :: ?int16
	,nextRestTime=[] :: ?int32
	,restProtectTimes=[] :: ?int16
	,stonechipStateList=[] :: [#p_stonechip{}]
	,tenfight_unlock_level=[] :: ?int16}).
-record(cs_plunder_compose,{
	targetTypeID=[] :: ?int32}).
-record(sc_plunder_compose,{
	result=[] :: ?int8
	,newStonechipState=[] :: [#p_stonechip{}]}).
-record(cs_plunder_get_target,{
	targetTypeID=[] :: ?int32
	,position=[] :: ?int32}).
-record(p_plunder_tar,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int16
	,roleName=[] :: ?string
	,title=[] :: ?int8
	,head=[] :: ?int32
	,fightPower=[] :: ?int64
	,probabilityType=[] :: ?int16
	,isRobot=[] :: boolean()}).
-record(sc_plunder_get_target,{
	result=[] :: ?int8
	,targetList=[] :: [#p_plunder_tar{}]}).
-record(cs_plunder_fight,{
	tarRoleID=[] :: ?int32
	,targetTypeID=[] :: ?int16
	,position=[] :: ?int8}).
-record(sc_plunder_fight,{
	roleID=[] :: ?int32
	,result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,rewardtargetTypeID=[] :: ?int16
	,add_times=[] :: ?int16
	,buy_price=[] :: ?int16
	,can_buy=[] :: ?int16
	,position=[] :: ?int8}).
-record(cs_plunder_use_protect,{
	}).
-record(sc_plunder_use_protect,{
	result=[] :: ?int8}).
-record(cs_plunder_buy_attacktime,{
	}).
-record(sc_plunder_buy_attacktime,{
	result=[] :: ?int8}).
-record(sc_plunder_notice_attacktime,{
	restAttackTimes=[] :: ?int16
	,nextRestTime=[] :: ?int32}).
-record(cs_plunder_fight_ten,{
	tarRoleID=[] :: ?int32
	,targetTypeID=[] :: ?int16
	,position=[] :: ?int8}).
-record(p_plunder_fight_mystery,{
	reward=[] :: [#p_reward_view{}]}).
-record(sc_plunder_fight_ten,{
	roleID=[] :: ?int32
	,result=[] :: ?int8
	,fight_result=[] :: [boolean()]
	,rewardtargetTypeID=[] :: ?int16
	,add_times=[] :: ?int16
	,buy_price=[] :: ?int16
	,can_buy=[] :: ?int16
	,position=[] :: ?int8
	,mystery_list=[] :: [#p_plunder_fight_mystery{}]}).
-record(cs_plunder_protect_time,{
	}).
-record(sc_plunder_protect_time,{
	startH=[] :: ?int8
	,stopH=[] :: ?int8}).
-record(cs_plunder_multi_compose,{
	targetTypeID=[] :: ?int32
	,num=[] :: ?int8}).
-record(sc_plunder_multi_compose,{
	result=[] :: ?int8
	,newStonechipState=[] :: [#p_stonechip{}]}).
-record(cs_pvp_get_list,{
	}).
-record(p_pvp,{
	roleID=[] :: ?int32
	,isMale=[] :: boolean()
	,level=[] :: ?int16
	,title=[] :: ?int8
	,roleName=[] :: ?string
	,fightPower=[] :: ?int64
	,rank=[] :: ?int16
	,head=0 :: ?int32
	,vip=[] :: ?int8}).
-record(sc_pvp_get_list,{
	rank=[] :: ?int16
	,pvpList=[] :: [#p_pvp{}]
	,max_rank=[] :: ?int16}).
-record(cs_pvp_fight,{
	roleID=[] :: ?int32
	,rank=[] :: ?int16}).
-record(sc_pvp_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,newRank=[] :: ?int16
	,pvppoint=[] :: ?int32
	,reputation=[] :: ?int32
	,gold=[] :: ?int32}).
-record(cs_pvp_get_first_eight_replays,{
	}).
-record(p_pvp_replay_info,{
	attackerName=[] :: ?string
	,defenderName=[] :: ?string
	,attackerNewRank=[] :: ?int16
	,defenderNewRank=[] :: ?int16
	,replayUID=[] :: ?int64
	,time=[] :: ?int32}).
-record(sc_pvp_get_first_eight_replays,{
	infoList=[] :: [#p_pvp_replay_info{}]}).
-record(cs_pvp_eight_replay,{
	replayUID=[] :: ?int64}).
-record(sc_pvp_eight_replay,{
	result=[] :: ?int8
	,fightInfo=[] :: #sc_fight_request{}}).
-record(cs_pvp_free_fight,{
	roleID=[] :: ?int32}).
-record(sc_pvp_free_fight,{
	result=[] :: ?int8
	,fightInfo=[] :: #sc_fight_request{}
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_pvp_get_free_fight_level,{
	}).
-record(sc_pvp_get_free_fight_level,{
	need_level=[] :: ?int32}).
-record(cs_shop_buy_num,{
	}).
-record(p_shop_num,{
	shopID=[] :: ?int16
	,sellID=[] :: ?int16
	,buyNum=[] :: ?int16
	,buyMax=[] :: ?int16
	,refresh_type=[] :: ?int8}).
-record(sc_shop_buy_num,{
	shopNumList=[] :: [#p_shop_num{}]}).
-record(cs_shop_buy,{
	shopID=[] :: ?int16
	,sellID=[] :: ?int16
	,num=[] :: ?int8}).
-record(sc_shop_buy,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view{}]}).
-record(cs_shop_encounter,{
	}).
-record(p_shop_random,{
	shopID=[] :: ?int16
	,refreshSec=[] :: ?int32
	,sellIDList=[] :: [?int16]}).
-record(sc_shop_encounter,{
	shopList=[] :: [#p_shop_random{}]
	,unioncoin_refresh=[] :: ?int32}).
-record(sc_shop_new,{
	newShop=[] :: #p_shop_random{}}).
-record(cs_shop_refresh,{
	shopID=[] :: ?int16}).
-record(sc_shop_refresh,{
	result=[] :: ?int8
	,newShop=[] :: [#p_shop_random{}]}).
-record(sc_shop_auto_refresh,{
	updateShop=[] :: #p_shop_random{}}).
-record(cs_shop_family_limit_info,{
	}).
-record(p_shop_family_limit,{
	shopID=[] :: ?int8
	,levelLimit=[] :: ?int8
	,buyTimesLimit=[] :: ?int16
	,usedTimes=[] :: ?int16
	,costUnionCoin=[] :: ?int16
	,reward=[] :: #p_reward_info{}}).
-record(sc_shop_family_limit_info,{
	result=[] :: ?int8
	,shop=[] :: [#p_shop_family_limit{}]}).
-record(cs_shop_family_limit_buy,{
	shopID=[] :: ?int8}).
-record(sc_shop_family_limit_buy,{
	result=[] :: ?int8}).
-record(cs_shop_seed_info,{
	}).
-record(seed_sell_info,{
	type=[] :: ?int16
	,cost_num=[] :: ?int32
	,seed_item=[] :: ?int32
	,harvest_num=[] :: ?int32}).
-record(sc_shop_seed_info,{
	shop_ist=[] :: [#seed_sell_info{}]}).
-record(cs_item_bag,{
	}).
-record(p_item,{
	itemUID=[] :: ?int64
	,itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int16
	,itemRank=[] :: ?int8
	,itemNum=[] :: ?int16
	,itemDecay=[] :: ?int32
	,itemExp=[] :: ?int16
	,itemenchantType=[] :: ?int8
	,itemenchantLevel=[] :: ?int8
	,itemLegendRank=[] :: ?int8}).
-record(sc_item_bag,{
	allItem=[] :: [#p_item{}]}).
-record(sc_item_bag2,{
	seq=[] :: ?int8
	,subSeq=[] :: ?int8
	,itemList=[] :: [#p_item{}]}).
-record(cs_item_equip,{
	}).
-record(sc_item_equip,{
	allEquip=[] :: [#p_equip{}]}).
-record(cs_item_sell,{
	itemUIDList=[] :: [?int64]}).
-record(sc_item_sell,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view3{}]
	,gold=[] :: ?int32}).
-record(cs_item_down_equip,{
	gerID=[] :: ?int64
	,itemPos=[] :: ?int8}).
-record(sc_item_down_equip,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,itemPos=[] :: ?int8}).
-record(cs_item_up_equip,{
	gerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemUID=[] :: ?int64
	,itemGerID=[] :: ?int64}).
-record(sc_item_up_equip,{
	result=[] :: ?int8
	,gerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemUID=[] :: ?int64}).
-record(sc_item_new,{
	newItemList=[] :: [#p_item{}]}).
-record(p_item_num_update,{
	itemUID=[] :: ?int64
	,itemNum=[] :: ?int16}).
-record(sc_item_update,{
	updateList=[] :: [#p_item_num_update{}]}).
-record(cs_item_use,{
	itemUID=[] :: ?int64
	,itemNum=[] :: ?int8}).
-record(sc_item_use,{
	result=[] :: ?int8
	,itemUID=[] :: ?int64
	,itemNum=[] :: ?int8}).
-record(sc_item_delete_notify,{
	itemUIDList=[] :: [?int64]}).
-record(cs_item_reinforce,{
	itemUID=[] :: ?int64
	,gerID=[] :: ?int64}).
-record(sc_item_reinforce,{
	result=[] :: ?int8
	,itemUID=[] :: ?int64
	,newLevel=[] :: ?int16}).
-record(cs_item_max_reinforce,{
	itemUID=[] :: ?int64
	,gerID=[] :: ?int64}).
-record(sc_item_max_reinforce,{
	result=[] :: ?int8
	,itemUID=[] :: ?int64
	,tempLevelList=[] :: [?int16]}).
-record(sc_item_update_rank,{
	itemUID=[] :: ?int64
	,newItemRank=[] :: ?int8
	,newItemDecay=[] :: ?int32}).
-record(cs_item_up_rank,{
	srcItemUID=[] :: ?int64
	,foodItemUID=[] :: ?int64
	,srcItemGerID=[] :: ?int64
	,foodItemGerID=[] :: ?int64}).
-record(sc_item_up_rank,{
	result=[] :: ?int8
	,srcItemUID=[] :: ?int64
	,foodItemUID=[] :: ?int64
	,newItemLevel=[] :: ?int16
	,newItemRank=[] :: ?int8}).
-record(cs_item_compound,{
	typeID=[] :: ?int16}).
-record(sc_item_compound,{
	result=[] :: ?int8
	,typeID=[] :: ?int16}).
-record(cs_item_eat,{
	itemID=[] :: ?int64
	,itemGerID=[] :: ?int64
	,foodItemIDList=[] :: [?int64]}).
-record(sc_item_eat,{
	result=[] :: ?int8
	,itemID=[] :: ?int64
	,newItemRank=[] :: ?int8
	,itemExp=[] :: ?int16}).
-record(p_all_equipment,{
	all_equipment_id=[] :: ?int32
	,all_equipment_list=[] :: [?int32]}).
-record(sc_item_all_equipment,{
	gerID=[] :: [?int64]
	,all_equipment_info_list=[] :: [#p_all_equipment{}]}).
-record(cs_item_use_info,{
	}).
-record(p_item_use_info,{
	type_id=[] :: ?int16
	,left_times=[] :: ?int8}).
-record(sc_item_use_info,{
	use_info_list=[] :: [#p_item_use_info{}]}).
-record(cs_item_auto_up_equip,{
	gerID=[] :: ?int64
	,type=[] :: ?int8}).
-record(sc_item_auto_up_equip,{
	result=[] :: ?int8}).
-record(cs_item_stone_eat,{
	stoneID=[] :: ?int64
	,gerID=[] :: ?int64
	,foodStoneIDList=[] :: [?int64]}).
-record(sc_item_stone_eat,{
	result=[] :: ?int8
	,stone_info=[] :: #p_item{}
	,itemGerID=[] :: ?int64
	,itemPos=[] :: ?int8}).
-record(p_item_decompose_unit,{
	decomposetype=[] :: ?int8
	,itemUID=[] :: ?int64}).
-record(cs_item_decompose,{
	itemUnitList=[] :: [#p_item_decompose_unit{}]}).
-record(sc_item_decompose,{
	result=[] :: ?int8
	,decomposeID=[] :: ?int64
	,rewardList=[] :: [#p_reward_view{}]}).
-record(cs_item_decompose_again,{
	decomposeID=[] :: ?int64}).
-record(sc_item_decompose_again,{
	result=[] :: ?int8
	,decomposeID=[] :: ?int64
	,rewardList=[] :: [#p_reward_view{}]}).
-record(cs_item_decompose_by_money_cost,{
	}).
-record(sc_item_decompose_by_money_cost,{
	result=[] :: ?int8
	,cost=[] :: ?int32}).
-record(cs_item_decompose_again_cost,{
	}).
-record(sc_item_decompose_again_cost,{
	result=[] :: ?int8
	,costlist=[] :: [#p_reward_view{}]}).
-record(cs_item_enchant,{
	type=[] :: ?int8
	,gerID=[] :: ?int64
	,itemUID=[] :: ?int64}).
-record(p_equip2,{
	itemUID=[] :: ?int64
	,itemTypeID=[] :: ?int16
	,itemLevel=[] :: ?int16
	,itemRank=[] :: ?int8
	,itemGerID=[] :: ?int64
	,itemPos=[] :: ?int8
	,itemDecay=[] :: ?int32
	,itemExp=[] :: ?int16
	,itemenchantType=[] :: ?int8
	,itemenchantLevel=[] :: ?int8}).
-record(sc_item_enchant,{
	result=[] :: ?int8
	,equip=[] :: #p_equip2{}}).
-record(cs_item_x_get,{
	targetTypeID=[] :: ?int16
	,itemIDs=[] :: [?int64]}).
-record(sc_item_x_get,{
	result=[] :: ?int8}).
-record(cs_item_down_rank,{
	itemUID=[] :: ?int64
	,srcGerUID=[] :: ?int64}).
-record(sc_item_down_rank,{
	result=[] :: ?int8
	,add_item_list=[] :: [#p_reward_view{}]
	,itemUID=0 :: ?int64
	,rank=0 :: ?int8}).
-record(cs_item_pft_uprank,{
	srcItemUID=[] :: ?int64
	,srcItemGerID=[] :: ?int64
	,foodItemUID=[] :: [?int64]}).
-record(sc_item_pft_uprank,{
	result=[] :: ?int8
	,newRank=0 :: ?int8
	,srcUID=0 :: ?int64
	,foodUIDs=[] :: [?int64]}).
-record(cs_item_stone_uprank,{
	srcStoneUID=[] :: ?int64
	,srcStoneGerID=[] :: ?int64
	,foodStoneUID=[] :: ?int64}).
-record(sc_item_stone_uprank,{
	result=[] :: ?int8
	,newstone=[] :: #p_item{}
	,srcStoneUID=[] :: ?int64
	,foodStoneUID=[] :: ?int64}).
-record(sc_item_dtl_update,{
	item=[] :: #p_equip2{}}).
-record(cs_item_max_reinforce_for_ger,{
	gerID=[] :: ?int64
	,type=[] :: ?int8}).
-record(p_reinforce_result,{
	equipID=[] :: ?int64
	,equipTypeID=[] :: ?int16
	,result=[] :: ?int8
	,tempLevelList=[] :: [?int16]}).
-record(sc_item_max_reinforce_for_ger,{
	gerID=[] :: ?int64
	,type=0 :: ?int8
	,resultlist=[] :: [#p_reinforce_result{}]}).
-record(cs_item_make_legend,{
	srcItemUID=[] :: ?int64
	,srcItemGerID=[] :: ?int64}).
-record(sc_item_make_legend,{
	result=[] :: ?int8
	,newLegendItemUID=[] :: ?int64
	,equipTypeID=[] :: ?int16}).
-record(cs_item_legend_uprank,{
	itemUID=[] :: ?int64
	,itemGerID=[] :: ?int64}).
-record(sc_item_legend_uprank,{
	result=[] :: ?int8
	,itemUID=0 :: ?int64
	,itemLegendRank=0 :: ?int8}).
-record(cs_item_stone_legend,{
	srcItemUID=[] :: ?int64
	,srcItemGerID=[] :: ?int64}).
-record(sc_item_stone_legend,{
	result=[] :: ?int8
	,newItemID=[] :: ?int64
	,srcItemID=[] :: ?int64
	,equipPos=[] :: ?int8
	,itemGerID=[] :: ?int64}).
-record(cs_explore_one,{
	}).
-record(p_echapter,{
	chapterID=[] :: ?int16
	,endTime=[] :: ?int32
	,value=[] :: ?int32
	,isCollected=[] :: boolean()}).
-record(sc_explore_one,{
	result=[] :: ?int8
	,chapterList=[] :: [#p_echapter{}]
	,roleExp=[] :: ?int32
	,gerExp=[] :: ?int32
	,coin=[] :: ?int32
	,crit=[] :: ?int8}).
-record(cs_explore_dungeon_list,{
	chapterID=[] :: ?int16}).
-record(p_edungeon,{
	dungeonID=[] :: ?int16
	,isPassed=[] :: boolean()}).
-record(sc_explore_dungeon_list,{
	chapterID=[] :: ?int16
	,dungeonList=[] :: [#p_edungeon{}]
	,state=[] :: ?int8}).
-record(cs_explore_challenge_encounter,{
	dungeonID=[] :: ?int16}).
-record(sc_explore_challenge_encounter,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward{}]
	,score=[] :: ?int8
	,state=[] :: ?int8}).
-record(sc_explore_delete_encounter,{
	chapterID=[] :: ?int16}).
-record(cs_explore_giveup_encounter,{
	chapterID=[] :: ?int16}).
-record(sc_explore_giveup_encounter,{
	result=[] :: ?int8}).
-record(cs_explore_list,{
	}).
-record(sc_explore_list,{
	chapterList=[] :: [#p_echapter{}]}).
-record(cs_explore_collect,{
	chapterID=[] :: ?int16}).
-record(sc_explore_collect,{
	chapterID=[] :: ?int16
	,result=[] :: ?int8}).
-record(cs_explore_force_collect,{
	chapterID=[] :: ?int16
	,type=[] :: ?int8}).
-record(sc_explore_force_collect,{
	chapterID=[] :: ?int16
	,type=[] :: ?int8
	,result=[] :: ?int8}).
-record(cs_explore_auto_explore_check,{
	}).
-record(sc_explore_auto_explore_check,{
	result=[] :: ?int8
	,needLevel=[] :: ?int16
	,allresult=[] :: ?int8
	,allneedLevel=[] :: ?int16}).
-record(cs_explore_encounter_pass_reward,{
	chapterID=[] :: ?int16}).
-record(sc_explore_encounter_pass_reward,{
	result=[] :: ?int8}).
-record(cs_explore_encounter_dungeon_state,{
	chapterID=[] :: ?int16}).
-record(sc_explore_encounter_dungeon_state,{
	result=[] :: ?int8
	,chapterID=[] :: ?int16
	,dungeonID=[] :: ?int16
	,state=[] :: ?int8}).
-record(cs_explore_free,{
	}).
-record(sc_explore_free,{
	result=[] :: ?int8}).
-record(cs_explore_all,{
	type=[] :: ?int8}).
-record(p_explore_reward,{
	roleExp=[] :: ?int32
	,gerExp=[] :: ?int32
	,coin=[] :: ?int32
	,crit=[] :: ?int8}).
-record(sc_explore_all,{
	result=[] :: ?int8
	,totalTimes=[] :: ?int32
	,reward=[] :: #p_reward_info{}}).
-record(cs_ger_info,{
	}).
-record(sc_ger_info,{
	gerList=[] :: [#p_ger{}]}).
-record(sc_ger_info2,{
	seq=[] :: ?int8
	,subSeq=[] :: ?int8
	,gerList=[] :: [#p_ger{}]}).
-record(p_ger_pos_info,{
	gerID=[] :: ?int64
	,gerPos=[] :: ?int8
	,itemUIDList=[] :: [?int64]
	,diamondList=[] :: [#p_diamond{}]}).
-record(sc_ger_update,{
	gerID=[] :: ?int64
	,gerQuality=[] :: ?int16
	,gerLevel=[] :: ?int16
	,gerAttack=[] :: ?int32
	,gerHpMax=[] :: ?int64
	,gerFightPower=[] :: ?int64
	,gerExp=[] :: ?int64
	,gerProMean=[] :: ?int64
	,gerDemageRate=[] :: [#p_demage_rate_unit{}]
	,gerSpeed=[] :: ?int16}).
-record(sc_ger_new,{
	newGer=[] :: #p_ger{}}).
-record(cs_ger_standup,{
	gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_ger_standup,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_move_pos,{
	gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(sc_ger_move_pos,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(cs_ger_pos_list,{
	}).
-record(p_ger_icon_unit,{
	pos=[] :: ?int8
	,icon=[] :: ?int8}).
-record(sc_ger_pos_list,{
	gerPosInfoList=[] :: [#p_ger_pos_info{}]
	,useTagID=[] :: ?int8
	,maxTagID=[] :: ?int8
	,iconList=[] :: [#p_ger_icon_unit{}]}).
-record(cs_ger_sell,{
	gerIDList=[] :: [?int64]}).
-record(sc_ger_sell,{
	result=[] :: ?int8
	,reward=[] :: [#p_reward_view3{}]}).
-record(cs_ger_detail,{
	gerID=[] :: ?int64}).
-record(sc_ger_detail,{
	gerID=[] :: ?int64
	,gerSpInit=[] :: ?int16
	,gerSpMax=[] :: ?int16
	,gerCritic=[] :: ?int16
	,gerCriticReduce=[] :: ?int16
	,gerDoom=[] :: ?int16
	,gerMiss=[] :: ?int16
	,gerAbsorb=[] :: ?int16
	,gerDamageBack=[] :: ?int16
	,gerReel=[] :: ?int16
	,gerReelReduce=[] :: ?int16
	,gerPhyDefBite=[] :: ?int16
	,gerPhyDef=[] :: ?int16
	,gerMagDefBite=[] :: ?int16
	,gerMagDef=[] :: ?int16
	,gerSpeed=[] :: ?int16
	,gerBaseSpeedGrow=[] :: ?int32}).
-record(cs_ger_view_other,{
	tarRoleID=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(sc_ger_view_other,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger{}]}).
-record(cs_ger_view_other_dtl,{
	tarRoleID=[] :: ?int32
	,serverID=[] :: ?int16}).
-record(sc_ger_view_other_dtl,{
	tarRoleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,roleLevel=[] :: ?int16
	,fightPower=[] :: ?int64
	,gerList=[] :: [#p_ger{}]
	,equipList=[] :: [#p_equip{}]
	,gerPosList=[] :: [#p_ger_pos{}]
	,lieuViewList=[] :: [#p_lieu_view{}]
	,head=[] :: ?int32
	,title=[] :: ?int8
	,trID=[] :: ?int32
	,specialID=[] :: ?int32
	,skinInfo=[] :: #p_skin_info{}
	,gerCrystalList=[] :: [#ger_crystalinfo_brief{}]}).
-record(sc_ger_update_exp,{
	gerID=[] :: ?int64
	,gerExp=[] :: ?int64}).
-record(cs_ger_eat,{
	gerID=[] :: ?int64
	,itemTypeID=[] :: ?int32
	,itemNum=[] :: ?int32}).
-record(sc_ger_eat,{
	result=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_up_rank,{
	srcGerID=[] :: ?int64
	,foodGerID=[] :: ?int64}).
-record(sc_ger_up_rank,{
	result=[] :: ?int8
	,srcGerID=[] :: ?int64
	,foodGerID=[] :: ?int64}).
-record(sc_ger_update_standlist,{
	posList=[] :: [#p_ger_pos{}]}).
-record(sc_ger_del,{
	gerIDList=[] :: [?int64]}).
-record(p_ger_power,{
	pos=[] :: ?int8
	,fightPower=[] :: ?int64}).
-record(sc_ger_refresh_power,{
	gerPowerList=[] :: [#p_ger_power{}]}).
-record(p_ger_save_item,{
	pos=[] :: ?int8
	,itemUID=[] :: ?int64}).
-record(p_ger_save_dtl,{
	pos=[] :: ?int8
	,gerUID=[] :: ?int64
	,itemList=[] :: [#p_ger_save_item{}]
	,diamondList=[] :: [#p_ger_save_item{}]}).
-record(p_ger_tag_dtl,{
	tag=[] :: ?int8
	,gerList=[] :: [#p_ger_save_dtl{}]
	,lieuList=[] :: [#p_ger_save_dtl{}]}).
-record(cs_ger_saved_tag,{
	tagID=[] :: ?int8}).
-record(sc_ger_saved_tag,{
	saveList=[] :: [#p_ger_tag_dtl{}]}).
-record(cs_ger_set_icon,{
	iconList=[] :: [#p_ger_icon_unit{}]}).
-record(sc_ger_set_icon,{
	}).
-record(cs_ger_buy_tag,{
	tagID=[] :: ?int8}).
-record(sc_ger_buy_tag,{
	result=[] :: ?int8}).
-record(cs_ger_set_tag_data,{
	tagID=[] :: ?int8
	,gerList=[] :: [#p_ger_save_dtl{}]
	,lieuList=[] :: [#p_ger_save_dtl{}]}).
-record(sc_ger_set_tag_data,{
	result=[] :: ?int8}).
-record(cs_ger_change_tag,{
	tagID=[] :: ?int8}).
-record(sc_ger_change_tag,{
	result=[] :: ?int8
	,gerPosInfoList=[] :: [#p_ger_pos_info{}]
	,lieuPosInfoList=[] :: [#p_ger_pos_info{}]}).
-record(cs_ger_lieu_pos_list,{
	}).
-record(sc_ger_lieu_pos_list,{
	gerPosInfoList=[] :: [#p_ger_pos_info{}]
	,isStart=[] :: ?int8}).
-record(cs_ger_lieu_standup,{
	gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_ger_lieu_standup,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_lieu_dequeue,{
	gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(sc_ger_lieu_dequeue,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,gerID=[] :: ?int64}).
-record(cs_ger_lieu_untie,{
	gerPos=[] :: ?int8}).
-record(p_ger_lieu_info,{
	gerPos=[] :: ?int8
	,specialID=[] :: ?int16
	,isLock1=[] :: ?int8
	,attAddID=[] :: ?int16
	,isLock2=[] :: ?int8
	,hpAddID=[] :: ?int16
	,isLock3=[] :: ?int8}).
-record(sc_ger_lieu_untie,{
	result=[] :: ?int8
	,info=[] :: [#p_ger_lieu_info{}]}).
-record(cs_ger_lieu_info_list,{
	}).
-record(sc_ger_lieu_info_list,{
	info=[] :: [#p_ger_lieu_info{}]}).
-record(cs_ger_lieu_move_pos,{
	gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(sc_ger_lieu_move_pos,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8
	,targetPos=[] :: ?int8}).
-record(cs_ger_lieu_lock_clo,{
	gerPos=[] :: ?int8
	,num=[] :: ?int8}).
-record(sc_ger_lieu_lock_clo,{
	result=[] :: ?int8}).
-record(cs_ger_lieu_unlock_clo,{
	gerPos=[] :: ?int8
	,num=[] :: ?int8}).
-record(sc_ger_lieu_unlock_clo,{
	result=[] :: ?int8}).
-record(cs_ger_lieu_refresh_clo,{
	gerPos=[] :: ?int8}).
-record(sc_ger_lieu_refresh_clo,{
	result=[] :: ?int8
	,info=[] :: #p_ger_lieu_info{}}).
-record(cs_ger_lieu_tie_info,{
	}).
-record(sc_ger_lieu_tie_info,{
	posList=[] :: [?int8]}).
-record(cs_ger_lieu_refresh_freeTimes,{
	}).
-record(sc_ger_lieu_refresh_freeTimes,{
	times=[] :: ?int16}).
-record(sc_ger_new_list,{
	newGerList=[] :: [#p_ger{}]}).
-record(cs_ger_down_rank,{
	srcGerID=[] :: ?int64}).
-record(sc_ger_down_rank,{
	result=[] :: ?int8
	,add_item_list=[] :: [#p_reward_view{}]
	,gerID=[] :: ?int64}).
-record(cs_ger_unload,{
	gerPos=[] :: ?int8}).
-record(sc_ger_unload,{
	result=[] :: ?int8
	,gerPos=[] :: ?int8}).
-record(cs_ger_mirror_info,{
	}).
-record(sc_ger_mirror_info,{
	srcGerTypeID=[] :: ?int64
	,newGerTypeID=[] :: ?int16
	,need_gold=[] :: [?int32]}).
-record(cs_ger_mirror_convert,{
	gerID=[] :: ?int64}).
-record(sc_ger_mirror_convert,{
	result=[] :: ?int8
	,newGerTypeID=[] :: ?int16}).
-record(cs_ger_mirror_commit,{
	operation_type=[] :: ?int8}).
-record(sc_ger_mirror_commit,{
	result=[] :: ?int8
	,newGerUID=[] :: ?int64}).
-record(cs_ger_set_body,{
	gerID=[] :: ?int64
	,gerBody=[] :: ?int8}).
-record(sc_ger_set_body,{
	result=[] :: ?int8}).
-record(cs_ger_second_uprank,{
	srcGerID=[] :: ?int64
	,foodGerIDList=[] :: [?int64]}).
-record(sc_ger_second_uprank,{
	result=[] :: ?int8
	,srcGerID=[] :: ?int64}).
-record(cs_ger_batch_uprank,{
	srcGerID=[] :: ?int64
	,foodGerIDList=[] :: [?int64]}).
-record(sc_ger_batch_uprank,{
	result=[] :: ?int8
	,srcGerID=[] :: ?int64}).
-record(cs_ger_second_transmigration,{
	srcGerID=[] :: ?int64}).
-record(sc_ger_second_transmigration,{
	result=[] :: ?int8}).
-record(cs_ger_transform_blink,{
	devourID=[] :: ?int64
	,devouredID=[] :: ?int64}).
-record(sc_ger_transform_blink,{
	result=[] :: ?int8
	,newblinkger=[] :: #p_ger{}}).
-record(cs_message_notice,{
	curMaxNoticeID=[] :: ?int32}).
-record(p_notice,{
	noticeID=[] :: ?int32
	,title=[] :: ?string
	,content=[] :: ?string}).
-record(sc_message_notice,{
	noticeIDList=[] :: [?int32]
	,noticeList=[] :: [#p_notice{}]
	,notice=[] :: ?string}).
-record(cs_message_certain_notice,{
	noticeIDList=[] :: ?int32}).
-record(sc_message_certain_notice,{
	noticeList=[] :: [#p_notice{}]}).
-record(sc_message_bc,{
	msg=[] :: ?string}).
-record(sc_message_bc_id,{
	msgID=[] :: ?int16}).
-record(sc_message_bc_id2,{
	msgID=[] :: ?int16
	,paramList=[] :: [any()]}).
-record(cs_message_test,{
	msg=[] :: ?string}).
-record(sc_message_test,{
	result=[] :: ?int8
	,errorMsg=[] :: ?string}).
-record(sc_message_best_card,{
	roleName=[] :: ?string
	,type=[] :: ?int8
	,value=[] :: ?int32}).
-record(sc_message_ger_upLevel,{
	roleName=[] :: ?string
	,gerInfo=[] :: #p_ger_view{}}).
-record(sc_message_item_uprank,{
	roleName=[] :: ?string
	,itemInfo=[] :: #p_item_view{}}).
-record(cs_battle_progress,{
	}).
-record(p_battle_chapter_star_reward,{
	chapterID=[] :: ?int16
	,totalScore=[] :: ?int16
	,rewardStatus=[] :: ?int16
	,hasbossreward=[] :: ?int8}).
-record(p_battle_progress,{
	type=[] :: ?int8
	,dungeonID=[] :: ?int16
	,chapterID=[] :: ?int16
	,dungeonCount=[] :: ?int16}).
-record(sc_battle_progress,{
	bpList=[] :: [#p_battle_progress{}]
	,bestPassChapterID=[] :: [?int16]
	,chapterRewardList=[] :: [#p_battle_chapter_star_reward{}]}).
-record(cs_battle_info,{
	type=[] :: ?int8
	,chapterID=[] :: ?int16}).
-record(p_dungeon,{
	dungeonID=[] :: ?int16
	,restTimes=[] :: ?int16
	,bestScore=[] :: ?int8
	,resetTimes=[] :: ?int16}).
-record(sc_battle_info,{
	type=[] :: ?int8
	,chapterID=[] :: ?int16
	,perfectRewarded=[] :: boolean()
	,dungeonInfo=[] :: [#p_dungeon{}]
	,dungeonCount=[] :: ?int16
	,starRewarded=[] :: ?int16}).
-record(cs_battle_challenge,{
	type=[] :: ?int8
	,dungeonID=[] :: ?int16}).
-record(sc_battle_challenge,{
	result=[] :: ?int8
	,fightInfo=[] :: [#sc_fight_request{}]
	,reward=[] :: [#p_reward{}]
	,score=[] :: ?int8}).
-record(cs_battle_perfect_reward,{
	chapterID=[] :: ?int16}).
-record(sc_battle_perfect_reward,{
	result=[] :: ?int8}).
-record(sc_battle_broadcast_get_item,{
	roleName=[] :: ?string
	,itemTypeID=[] :: ?int32
	,num=[] :: ?int8
	,dungeonID=[] :: ?int16
	,chapterID=[] :: ?int16}).
-record(cs_battle_dungeon_raids,{
	dungeonID=[] :: ?int16}).
-record(sc_battle_dungeon_raids,{
	result=[] :: ?int8
	,raidsTimes=[] :: ?int8
	,reward=[] :: [#p_reward{}]}).
-record(cs_battle_star_reward,{
	chapterID=[] :: ?int16
	,rewardID=[] :: ?int16}).
-record(sc_battle_star_reward,{
	result=[] :: ?int8
	,starRewarded=[] :: ?int16
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_battle_reset_dungeon,{
	type=[] :: ?int8
	,dungeonID=[] :: ?int16}).
-record(sc_battle_reset_dungeon,{
	result=[] :: ?int8
	,restTimes=[] :: ?int16}).
-record(sc_battle_battle_fail,{
	result=[] :: ?int8
	,cost=[] :: ?int16
	,restResetTimes=[] :: ?int16}).
-record(cs_battle_world_level,{
	}).
-record(sc_battle_world_level,{
	maxLevel=[] :: ?int16
	,n=[] :: ?int16
	,diff=[] :: ?int16}).
-record(cs_battle_obtain_boss_reward,{
	type=[] :: ?int8
	,chapterID=[] :: ?int16
	,dungeonID=[] :: ?int16}).
-record(sc_battle_obtain_boss_reward,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,chapterID=[] :: ?int16
	,dungeonID=[] :: ?int16
	,reward=[] :: [#p_reward_info{}]}).
-record(cs_battle_get_boss_reward_info,{
	type=[] :: ?int8
	,chapterid=[] :: ?int16
	,dungeonid=[] :: ?int16}).
-record(sc_battle_get_boss_reward_info,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,chapterid=[] :: ?int16
	,dungeonid=[] :: ?int16}).
-record(cs_battle_dojang_info,{
	}).
-record(p_dojang_info,{
	index=[] :: ?int8
	,state=[] :: ?int8}).
-record(sc_battle_dojang_info,{
	fight_time=[] :: ?int32
	,already_buy_time=[] :: ?int32
	,dojang_list=[] :: [#p_dojang_info{}]
	,can_buy_time=[] :: ?int32}).
-record(cs_battle_dojang_fight,{
	index=[] :: ?int8}).
-record(sc_battle_dojang_fight,{
	result=[] :: ?int8
	,dojang_info=[] :: #p_dojang_info{}
	,rewards=[] :: [#p_reward_info{}]
	,fightInfo1=[] :: [#sc_fight_request{}]
	,fightInfo2=[] :: [#sc_fight_double_request{}]}).
-record(cs_battle_dojang_harvest,{
	index=[] :: ?int8}).
-record(sc_battle_dojang_harvest,{
	result=[] :: ?int8
	,rewards=[] :: [#p_reward_info{}]}).
-record(cs_battle_dojang_buy,{
	buy_time=[] :: ?int32}).
-record(sc_battle_dojang_buy,{
	result=[] :: ?int8
	,new_fight_time=[] :: ?int32
	,already_buy_time=[] :: ?int32
	,can_buy_time=[] :: ?int32}).
-record(cs_role_info,{
	}).
-record(sc_role_info,{
	roleID=[] :: ?int32
	,roleName=[] :: ?string
	,isMale=[] :: boolean()
	,description=[] :: ?string
	,familyID=[] :: ?int32
	,level=[] :: ?int16
	,exp=[] :: ?int64
	,coin=[] :: ?int64
	,reputation=[] :: ?int32
	,gold=[] :: ?int32
	,goldBonus=[] :: ?int32
	,goldUsed=[] :: ?int32
	,vipLevel=[] :: ?int8
	,goldTotalPaid=[] :: ?int32
	,energy=[] :: ?int16
	,energyBuyTimes=[] :: ?int16
	,nextEnergyTime=[] :: ?int32
	,discoveryTimes=[] :: ?int16
	,nextDscvTime=[] :: ?int32
	,pvpTimes=[] :: ?int16
	,plunderTimes=[] :: ?int16
	,randomPVPTimes=[] :: ?int8
	,singlePVPTimes=[] :: ?int8
	,title=[] :: ?int8
	,encounterFreeNum=[] :: ?int8
	,isPVPPushOpen=[] :: boolean()
	,isPushNightMute=[] :: boolean()
	,dscvBuyTimes=[] :: ?int16
	,pvpBuyTimes=[] :: ?int16
	,plunderBuyTimes=[] :: ?int16
	,coinBuyTimes=[] :: ?int16
	,weiboCount=[] :: ?int8
	,nextPvpTime=[] :: ?int32
	,nextPlunderTime=[] :: ?int32
	,challengeGodEnergy=[] :: ?int16
	,challengeGodBuyTimes=[] :: ?int16
	,lastWeiXinShareSec=[] :: ?int32
	,head=[] :: ?int32
	,payExtReward=[] :: ?int32
	,isFailed=[] :: boolean()
	,alienTimes=[] :: ?int8
	,lastAlienTime=[] :: ?int32
	,unioncoin=[] :: ?int32
	,talentstudyBuyTimes=[] :: ?int16
	,profoundCrystal=[] :: ?int32
	,honor=[] :: ?int32
	,pvppoint=[] :: ?int32
	,teamPkTimes=[] :: ?int16
	,teamPkBuyTimes=[] :: ?int16
	,nextTeamPkTime=[] :: ?int32
	,home_resource=[] :: ?int32
	,main_gertypeid=[] :: ?int16
	,ticket=[] :: ?int32
	,laputastone=[] :: ?int32
	,transmigration=[] :: ?int32
	,sGoldTotalPaid=[] :: ?int32
	,svipLevel=[] :: ?int8}).
-record(sc_role_update_level,{
	level=[] :: ?int16}).
-record(sc_role_update_list,{
	updateAttrList=[] :: [#sc_role_update_level{}]}).
-record(sc_role_update_unioncoin,{
	unioncoin=[] :: ?int32}).
-record(cs_role_buy_energy,{
	type=[] :: ?int8}).
-record(sc_role_buy_energy,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,newEnergy=[] :: ?int16
	,newBuyTimes=[] :: ?int16
	,crit=[] :: ?int8}).
-record(sc_role_update_exp,{
	exp=[] :: ?int64}).
-record(sc_role_update_coin,{
	coin=[] :: ?int64}).
-record(sc_role_update_reputation,{
	reputation=[] :: ?int32}).
-record(sc_role_update_gold,{
	gold=[] :: ?int32}).
-record(sc_role_update_goldBonus,{
	goldBonus=[] :: ?int32}).
-record(sc_role_update_vipLevel,{
	vipLevel=[] :: ?int8
	,challengeGodFree=[] :: ?int16
	,challengeGodBuy=[] :: ?int16}).
-record(sc_role_update_energy,{
	energy=[] :: ?int16
	,nextEnergyTime=[] :: ?int32}).
-record(sc_role_update_discoveryTimes,{
	discoveryTimes=[] :: ?int16
	,nextDscvTime=[] :: ?int32}).
-record(sc_role_update_pvpTimes,{
	pvpTimes=[] :: ?int16
	,nextPvpTime=[] :: ?int32}).
-record(sc_role_update_plunderTimes,{
	plunderTimes=[] :: ?int16
	,nextPlunderTime=[] :: ?int32}).
-record(sc_role_update_randomPVPTimes,{
	randomPVPTimes=[] :: ?int8}).
-record(sc_role_update_singlePVPTimes,{
	singlePVPTimes=[] :: ?int8}).
-record(sc_role_update_goldUsed,{
	goldUsed=[] :: ?int32}).
-record(sc_role_update_title,{
	title=[] :: ?int8}).
-record(sc_role_update_encounterFreeNum,{
	encounterFreeNum=[] :: ?int8}).
-record(sc_role_update_weiboCount,{
	weiboCount=[] :: ?int8}).
-record(cs_role_setting,{
	}).
-record(sc_role_setting,{
	idList=[] :: [?int8]}).
-record(cs_role_get_energy,{
	energy_step=[] :: ?int8}).
-record(sc_role_get_energy,{
	result=[] :: ?int8}).
-record(cs_role_buy_coin_value,{
	}).
-record(sc_role_buy_coin_value,{
	value=[] :: ?int32
	,times=[] :: ?int16}).
-record(cs_role_weixin_share,{
	}).
-record(sc_role_update_pay_ext,{
	pay_ext=[] :: ?int32}).
-record(cs_role_suggest_open,{
	}).
-record(sc_role_suggest_open,{
	is_open=[] :: boolean()}).
-record(cs_role_suggest,{
	title=[] :: ?string
	,content=[] :: ?string}).
-record(sc_role_suggest,{
	result=[] :: ?int8}).
-record(cs_role_log_guide_state,{
	value=[] :: ?int16}).
-record(sc_role_talent_remains_point,{
	remains_point=[] :: ?int16}).
-record(cs_role_multi_buy_energy,{
	type=[] :: ?int8
	,times=[] :: ?int16}).
-record(p_multi_buy_energy,{
	value=[] :: ?int32
	,crit=[] :: ?int16}).
-record(sc_role_multi_buy_energy,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,newEnergy=[] :: ?int16
	,newBuyTimes=[] :: ?int16
	,list=[] :: [#p_multi_buy_energy{}]}).
-record(cs_role_is_under_examine,{
	}).
-record(sc_role_is_under_examine,{
	result=[] :: boolean()}).
-record(cs_role_push_setting,{
	type=[] :: ?int8
	,value=[] :: ?int16}).
-record(sc_role_push_setting,{
	result=[] :: ?int8
	,type=[] :: ?int8
	,value=[] :: ?int16}).
-record(cs_role_get_guide_state,{
	}).
-record(sc_role_get_guide_state,{
	value=[] :: ?int16}).
-record(cs_role_set_guide_state,{
	value=[] :: ?int16}).
-record(sc_role_set_guide_state,{
	result=[] :: ?int8}).
-record(cs_role_change_head,{
	head=[] :: ?int32}).
-record(sc_role_change_head,{
	result=1 :: ?int8
	,head=0 :: ?int32}).
-record(cs_role_change_location,{
	location=[] :: ?string}).
-record(sc_role_update_teamPkTimes,{
	teamPkTimes=[] :: ?int16
	,nextTeamPkTime=[] :: ?int32}).
-record(cs_role_teamPkTimes_info,{
	}).
-record(sc_role_teamPkTimes_info,{
	times=[] :: ?int8
	,limit=[] :: ?int16
	,cost=[] :: [?int16]}).
-record(cs_role_token,{
	token=[] :: ?string}).
-record(sc_role_token,{
	}).
-record(cs_role_select_ger,{
	gerTypeID=[] :: ?int16}).
-record(sc_role_select_ger,{
	result=[] :: ?int8}).
-record(cs_role_demo_fight,{
	type=[] :: ?int8}).
-record(sc_role_demo_fight,{
	type=[] :: ?int8
	,fightInfo=[] :: #sc_fight_request{}}).
-record(sc_role_base_config,{
	energyMax=[] :: ?int8
	,dscvMax=[] :: ?int8}).
-record(cs_role_pay_ios,{
	receipt=[] :: ?string
	,payID=[] :: ?int32
	,deviceID=[] :: ?string
	,macAddr=[] :: ?string
	,type=[] :: ?int8}).
-record(sc_role_pay_ios,{
	result=[] :: ?int8
	,receipt=[] :: ?string
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(cs_role_pay_91,{
	receipt=[] :: ?string
	,payID=[] :: ?int32
	,deviceID=[] :: ?string
	,macAddr=[] :: ?string}).
-record(sc_role_pay_91,{
	result=[] :: ?int8
	,receipt=[] :: ?string
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_uc,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_dl,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_zz,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_360,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_wdj,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_dk,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_mi,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_pay_az,{
	result=[] :: ?int8
	,newGoldTotalPaid=[] :: ?int32
	,isGetFirstChargeReward=[] :: boolean()}).
-record(sc_role_update_profoundCrystal,{
	profoundCrystal=[] :: ?int32}).
-record(sc_role_update_honor,{
	honor=[] :: ?int32}).
-record(sc_role_update_pvppoint,{
	pvppoint=[] :: ?int32}).
-record(sc_role_update_home_resource,{
	home_resource=[] :: ?int32}).
-record(sc_role_update_ticket,{
	ticket=[] :: ?int32}).
-record(sc_role_update_laputastone,{
	laputastone=[] :: ?int32}).
-record(cs_role_do_transmigration,{
	}).
-record(sc_role_do_transmigration,{
	result=[] :: ?int32}).
-record(cs_role_can_transmigration,{
	}).
-record(sc_role_can_transmigration,{
	is_can=[] :: ?int32
	,battle_normal=[] :: ?int32
	,battle_hard=[] :: ?int32
	,need_level=[] :: ?int32
	,need_ger=[] :: ?int32
	,need_battle_normal=[] :: ?int32
	,need_battle_hard=[] :: ?int32}).
-record(sc_role_update_svip,{
	svipLevel=[] :: ?int8
	,sGoldPaidNow=[] :: ?int32}).
-record(cs_account_login,{
	userID=[] :: ?int32
	,unixTime=[] :: ?int32
	,accountName=[] :: ?string
	,ticket=[] :: ?string
	,macAddr=[] :: ?string
	,serverID=[] :: ?int16
	,deviceID=[] :: ?string
	,srcType=[] :: ?int16}).
-record(sc_account_login,{
	result=[] :: ?int16
	,isCreated=[] :: boolean()
	,isGerSelected=[] :: boolean()
	,guiCreateRole=[] :: ?int8}).
-record(cs_account_create,{
	roleName=[] :: ?string
	,sex=[] :: ?int8}).
-record(sc_account_create,{
	result=[] :: ?int8}).
-record(cs_account_enter_game,{
	}).
-record(sc_account_enter_game,{
	result=[] :: ?int8}).
-record(sc_account_kick,{
	reason=[] :: ?int8}).
-record(cs_account_heart,{
	}).
-record(sc_account_heart,{
	unixTime=[] :: ?int32}).
-record(cs_account_logout,{
	}).
-record(cs_account_check_rolename,{
	roleName=[] :: ?string}).
-record(sc_account_check_rolename,{
	result=[] :: ?int8}).
-record(cs_account_demo,{
	type=[] :: ?int8
	,isMale=[] :: ?int8
	,specialID=[] :: ?int16}).
-record(sc_account_demo,{
	fightInfo=[] :: #sc_fight_double_request{}}).
