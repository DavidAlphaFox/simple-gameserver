%% @author 
%% @doc double战斗接口
%% Created 2013-2-26

%% 		-4			-5			-6 
%%		-1			-2			-3
%%		f
%%
%%		 1			 2			 3
%%		 4			 5			 6
-module(double_fight).
-include("def_role.hrl").
-include("def_fight.hrl").
-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 战斗计算接口
test()->
	Fighters =  [{ger,2000000004166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
 ,{ger,2000000004109,{gerBase,7010,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,203998,1882210,100,100,10,15,15,10,0,0,15,15,36,0,1,0,80852,2403837},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1882210,100,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
 ,{ger,2000000004121,{gerBase,7050,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
 ,{ger,2000000004127,{gerBase,7070,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
	Talent = [{12000,40},{13000,40},{22000,30},{23000,30},{21000,15},{24000,15},{32000,10},{31000,10},{33000,10},{34000,10},{41000,20},{43000,25},{52000,40},{53000,40},{51000,25},{61000,20},{62000,15},{54000,35},{64000,20},{71000,32},{72000,15},{11000,1},{73000,15},{74000,31},{93000,51},{92000,50},{81000,5},{94000,7},{91000,9},{63000,20},{42000,14}],
	Spe = {trSpecial,10001,1001,251,0,0},
	Skin = {skin_info,[],0},
	Lieu = {10,10},
	Fighters2 =  [{ger,2000000004369,{gerBase,7220,20,100,6,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
 ,{ger,2000000004438,{gerBase,7110,20,100,1,0,[]},{gerAttr,2403837,2500,10000,2000,203998,1882210,100,100,10,15,15,10,0,0,15,15,36,0,1,0,80852,2403837},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1882210,100,2403837,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
 ,{ger,2000000004527,{gerBase,7150,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
 ,{ger,2000000004176,{gerBase,7270,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],

	
	T1 = #double_team{skin_info=Skin,talent=Talent,trainer=Spe,fighters=Fighters,lieu=Lieu},
	T2 = T1#double_team{fighters=Fighters2},
	Fighters3 = [{ger,2000000006166,{gerBase,7220,20,100,4,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
				 ,{ger,2000000004199,{gerBase,7290,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
				 ,{ger,2000000004198,{gerBase,7280,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
	
	Fighters4 = [{ger,2000000005166,{gerBase,7220,20,100,5,0,[]},{gerAttr,2543493,2500,10000,2000,224664,2337503,0,100,10,15,15,10,0,0,40,15,1,0,1,0,84374,2543493},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2337503,0,2543493,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
				 ,{ger,2000000004186,{gerBase,7330,20,100,3,0,[]},{gerAttr,2465117,2500,10000,2000,202559,2134523,0,200,10,15,15,10,0,3500,15,15,1,0,1,0,70314,2465117},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},2134523,0,2465117,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}
				 ,{ger,2000000004185,{gerBase,7320,20,100,2,0,[]},{gerAttr,2428329,2500,10000,2000,212718,1936579,0,100,10,15,15,10,2500,0,15,15,1,0,1,0,91846,2428329},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96497,96497,0,0,0,0,0,0,0,0},1936579,0,2428329,{add_attr,3883,38833,0,0,20,20,20,20,0,0,0,0,40,40,40,40,6000,6000,0,0,0,0,0,0,0,0},{add_attr,23700,60700,0,0,730,520,0,0,0,0,0,0,1040,610,1040,610,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},{add_attr,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}],
	
	T3 = T1#double_team{fighters=Fighters3},
	T4 = T1#double_team{fighters=Fighters4},
	new(T1,T2,T3,T4,false).

new(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,IsChange) ->
	Ref = erlang:make_ref(),%%防止收到其他来自其他战斗的战斗结果,加上REF后,确保后面取到的消息是该次战斗请求的结果
	new(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,self(),Ref,IsChange),
	get_result(Ref).


new(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,From,Ref,IsChange) ->
	new(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,[],From,Ref,IsChange).
new(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,DeadList,From,Ref,IsChange) ->
	spawn(fun()-> ?CATCH(start(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,DeadList, #arg_env{isChange=IsChange,from=From,ref=Ref})) end).
	

%% @doc 异步获取战斗结果
get_result(Ref) ->
	receive
		{fight_result,Ref,FightResult} ->
			FightResult
	after 2000 ->
			exit("unknown fight error")
	end.

%% @doc 战斗计算接口
%% 先把每个玩家的精灵按照玩家自己的buff进行累积
%% buff修正之后,进入enter_skill阶段,把释放技能的buff和其他buff叠加并修改属性数值
start(Team1Leader,Team1Fellow,Team2Leader,Team2Fellow,DeadList,Arg) ->
	random:seed(util:gen_random_seed()),
	{Team1Fighters,Tr1s} = init_fighter_data(1,Team1Leader,Team1Fellow),
	{Team2Fighters,Tr2s} = init_fighter_data(-1,Team2Leader,Team2Fellow),
	GerList = Team1Fighters++Team2Fighters,
	init_fight_dict(GerList),
	mark_reborn(DeadList),
	FighterList = gers2fighters(GerList),
    ?set_alives(FighterList),

    SAction = init_fighter_talent(Team1Leader#double_team.talent, Team2Leader#double_team.talent),
	enter_skill(FighterList,SAction,Arg#arg_env{attTr=Tr1s,dfdTr=Tr2s}).

gers2fighters(GerList) ->
	[begin
		 #gerBase{gerPos=Pos,gerQuality=GerQuality,gerTypeID=GerTypeID} = Ger#ger.gerBase,
		 Skills = ger_lib:get_ger_skills(GerTypeID,GerQuality),
		 #fighter_info{pos=Pos,skill=Skills}
	 end || Ger<-GerList].

init_fighter_talent(Team1Talent, Team2Talent) ->
    SAction1 = init_talent_hook(Team1Talent,true),
    SAction2 = init_talent_hook(Team2Talent,false),
    SAction1 ++ SAction2.

init_fighter_data(Sign,#double_team{skin_info=SkinInfo,trainer=Trainer1,itemList=ItemList1}=Team1,Team2=#double_team{skin_info=SkinInfo2,trainer=Trainer2,itemList=ItemList2})->
    %%初始化传奇锻造加成
    GerEquipList = role_item:assort_ger_equiplist(ItemList1++ItemList2),
	LegendAddList = [{GerID,ger_attr:get_ger_legend_add(EquipList)}||{GerID,EquipList}<-GerEquipList],
   	[?set_ger_legend_add(GerID,LegendAdd)||{GerID,LegendAdd}<-LegendAddList],
    %%v3.3.0皮肤属性会收到转生影响，此处强行使用玩家等级来判断转生
    #trSpecial{roleLevel=ARoleLevel} = Trainer1,
	%#trSpecial{roleLevel=DRoleLevel} = Trainer2,
	Transmigrate1 = case ARoleLevel >300 of true->1;_->0 end,
	%Transmigrate2 = case DRoleLevel >300 of true->1;_->0 end,
	SkinBuff = role_skin:calculate_skin_buff(SkinInfo,Transmigrate1),
	?set_skin_info(Sign,{SkinInfo,SkinInfo2}),
	?set_skin_buff(Sign,SkinBuff),
	Team1_new = init_fighters(Sign,Team1,leader),
	Team2_new = init_fighters(Sign,Team2,fellow),
	{Team1_new ++ Team2_new,{Trainer1,Trainer2}}.


init_fighters(Sign,#double_team{fighters=Fighters,lieu=Lieu},Type)->
	Fighters2 = change_pos(Sign,Fighters,Type),
	Fighters3 = [append_ger_enter_buffer(Fighter,role_fight:transformOldLieuAdd2NewLieuAdd(Lieu))||Fighter<-Fighters2],
	Fighters3.

change_pos(Sign,Fighters,leader) ->
	[Fighter#ger{gerBase=GerBase#gerBase{gerPos=Sign * GerPos}}||Fighter=#ger{gerBase=GerBase=#gerBase{gerPos=GerPos}}<-Fighters];
change_pos(Sign,Fighters,fellow) ->
	[Fighter#ger{gerBase=GerBase#gerBase{gerPos=Sign*(GerPos+10)}}||Fighter=#ger{gerBase=GerBase=#gerBase{gerPos=GerPos}}<-Fighters].
	
append_ger_enter_buffer(Ger=#ger{gerFamilyTekAdd=FTek, gerMagicBookAdd=MB,gerEnchantAdd=EB,gerAwakeAdd=AB,gerCrystalAdd=CrystalAdd},LieuAdd)->
	V0 = append_enter_skill_attr(Ger,LieuAdd),
	V1 = append_enter_skill_attr(V0,FTek),
	V2 = append_enter_skill_attr(V1,MB),
	V3 = append_enter_skill_attr(V2,EB),
	V4 = append_enter_skill_attr(V3,AB),
	append_enter_skill_attr(V4,CrystalAdd).

enter_skill([#fighter_info{pos=SrcGerPos,skill=#gerSkill{enter=GerEnterSkills}}|Tail],ActionList,ArgEnv)->
	SrcGer = ?get_ger(SrcGerPos),
	ActionList2 = 
		lists:foldl(fun(SkillIDEnter,ActionsListAcc) ->
							case data_skill:get(SkillIDEnter) of
								#data_skill{targetSelect=_TarSelect,attackActionID=AttackActionID,defendActionID=DefendActionID,gerEnterOrGodAdd=AddAttr}=DataSkill ->
									TarPosList=select_target(DataSkill,SrcGerPos),
									ActionsListAcc2 = case AttackActionID of
														  0 -> ActionsListAcc;
														  _ -> [?action(AttackActionID, SrcGerPos,TarPosList,0,0,?STATE_DEFAULT)|ActionsListAcc]
													  end,
									lists:foldl(fun(TarPos,DefendActionList)->
														Tar = ?get_ger(TarPos),
														Tar2 = append_enter_skill_attr(Tar,AddAttr),
														?set_ger(Tar2),
														if DefendActionID == 0 -> DefendActionList;
														   true ->[?action(DefendActionID,TarPos,[],0,0,?STATE_DEFAULT)||DefendActionList]
														end
												end,ActionsListAcc2,TarPosList);
								_ ->
									ActionsListAcc
							end
					end,ActionList,GerEnterSkills),
	AllAddAttr = get_awake_third_step_add_attr(SrcGer#ger.gerBase#gerBase.gerAwakeInfo,SrcGer#ger.gerBase#gerBase.gerCrystalInfo),
	lists:foreach(fun(TarPos)->
						  Tar = ?get_ger(TarPos),
						  Tar2 = append_enter_skill_attr(Tar,AllAddAttr),
						  ?set_ger(Tar2)
				  end,self_alive(SrcGerPos)),
	enter_skill(Tail,ActionList2,ArgEnv);
enter_skill([],ActionList,ArgEnv=#arg_env{attTr=AttackerTr,dfdTr=DefenderTr,isChange=IsChange})->
	%%为所有精灵加上传奇锻造加成,将会修改精灵的属性
	add_legend_buff_for_all_ger(),
	trainer_special_enter_skill(ArgEnv),
	apply_attr_and_record_god(),
	FighterList = [ger2p_fighter(?get_ger(Pos))||#fighter_info{pos=Pos}<-?get_alives()],
	AtkTr = trainers2p_fighter(AttackerTr,1,?get_skin_info(1)),
	DfdTr = trainers2p_fighter(DefenderTr,-1,?get_skin_info(-1)),
	FighterList2 = AtkTr ++ DfdTr ++ FighterList,
	ArgEnv1 = ArgEnv#arg_env{fighterList=FighterList2},
	StandActions = do_stand_fighters(IsChange),
	ActionList2 = enter_hook(StandActions++ActionList),
	ActionList3 = [?action(?ACTION_LOOP_START,0,[],0,0,?STATE_DEFAULT)| ActionList2],
	case do_trainer_action0(ArgEnv1,0,ActionList3) of
		{ActionList4,ArgEnv2} ->
			fight_loop(sort_fighters(?get_queue()),0,ActionList4,ArgEnv2);
		_ ->
			ignore
	end.

do_stand_fighters(IsChange)->
	FQL = if IsChange -> [-1,1,-2,2,-3,3,-4,4,-5,5,-6,6];true ->[1,-1,2,-2,3,-3,4,-4,5,-5,6,-6] end,
	{Queue,Alives,Action} = lists:foldl(
			  fun(FPos,{QueueAcc,AlivesAcc,ActionAcc}) ->
					  case lists:keyfind(FPos, #fighter_info.pos, AlivesAcc) of
						  false ->
							  FPos2 = if FPos > 0 -> FPos + 10;true -> FPos - 10 end,
							  case lists:keytake(FPos2,#fighter_info.pos,AlivesAcc) of
								  false ->
									  {QueueAcc,AlivesAcc,ActionAcc};
								  {value,FellowFighter,OtherAlives} ->
									  change_fight_dict(FPos2,FPos),
									  Fellow2 = FellowFighter#fighter_info{pos=FPos,speed=(?get_ger(FPos))#ger.gerSpeed},
									  {[Fellow2|QueueAcc],[Fellow2|OtherAlives],[?action(16, FPos2, [FPos2], 0, 0, ?STATE_DEFAULT)|ActionAcc]}
							  end;
						  LeaderFighter->
							  LeaderFighter2 = LeaderFighter#fighter_info{speed=(?get_ger(FPos))#ger.gerSpeed},
							  {[LeaderFighter2|QueueAcc],AlivesAcc,ActionAcc}
					  end
			  end,{[],?get_alives(),[]},FQL),
	?set_alives(Alives),
	?set_queue(lists:reverse(Queue)),
	Action.

% 调用前先delete掉阵上的精灵,然后stand_fellow来上阵替补,返回值是actions
stand_fellow(Pos) ->
	Ger = ?get_ger(Pos),
	case Ger#ger.gerHp =< 0 of
		true ->
			FPos2 = if Pos > 0-> Pos+10; true -> Pos - 10 end,
			case lists:keytake(FPos2, #fighter_info.pos, ?get_alives()) of
				false ->
					[];
				{value,FellowFighter, OtherAlives} ->
					change_fight_dict(FPos2,Pos),
					FellowFighter2 = FellowFighter#fighter_info{pos=Pos},
					?set_alives([FellowFighter2|OtherAlives]),
					NewQueue=lists:keyreplace(Pos, #fighter_info.pos, ?get_queue(), FellowFighter2),
					?set_queue(NewQueue),
					LastLoopQueue =?get_loop_queue(),
					case lists:keytake(Pos,#fighter_info.pos,LastLoopQueue) of
						false -> ignore;
						{_,_,_Others} ->
							%NewLoopQueue = sort_fighters([FellowFighter2|Others]),
							NewLoopQueue = sort_fighters(lists:keystore(Pos, #fighter_info.pos, LastLoopQueue, FellowFighter2)),
							?set_loop_queue(NewLoopQueue)
					end,
					[?action(16, FPos2, [FPos2], 0, 0, ?STATE_DEFAULT)]
			end;
		_ ->
			[]
	end.


init_param(Param) ->
	DefenderMinHp = Param#fi_param.d_minHp,
	set_d_minHp(DefenderMinHp),
	CurLimit = 100 - Param#fi_param.cur_limit,
	set_cur_limit(CurLimit).


trainer_special_enter_skill(#arg_env{attTr={AttackerTrL,AttackerTrF},dfdTr={DefenderTrL,DefenderTrF}}) ->
	Alives2 = [begin
				  #ger{gerBase=Base} = ?get_ger(Pos),
				  #data_ger{gerProperty=ID0} = data_ger:get(Base#gerBase.gerTypeID),
				  {Pos,ID0}
			  end||#fighter_info{pos=Pos}<-?get_alives(), abs(Pos) < 10],
	[trainer_special_enter_skill(Alives2, Tr,Pos)
	||{Tr,Pos}<-[{AttackerTrL,1},{DefenderTrL,-1},{AttackerTrF,2},{DefenderTrF,-2}]].

trainer_special_enter_skill(_,#trSpecial{specialID=0},_) ->
	[];
trainer_special_enter_skill(Alives,#trSpecial{specialID=SpecialID,roleLevel=Level},T) ->
	{ID1,ID2} = data_trSpecial:get({specialID,SpecialID}),
	SkillID = data_trSpecial:get({skill1,SpecialID}),
	#trBuff{modulusBase=Base,modulusCo=Co,special=Special,buff=Buff} =data_trSpecial:get({skill,SkillID}),
	Add = Base+Level*Co,
	put({?trSpecial,?sign(T)},{[ID1,ID2],Add}),
	[begin 
		 trainer_special_enter_skill(SkillID,Pos,Add,Buff)
	 end||{Pos,ID0}<-Alives,?sign(Pos)==?sign(T),case Special of 1 -> ID0==ID1 orelse ID0 == ID2; _ -> true end].

trainer_special_enter_skill(1,Pos,Add,_) ->
	#unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
	#unique_effect{cure_add=CureAdd} = UniqueEffect,
	UniqueEffect2 = UniqueEffect#unique_effect{cure_add=CureAdd + Add},
	?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2});
trainer_special_enter_skill(2,Pos,Add,_) ->
	#unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
	#unique_effect{attack_add=DamageAdd} = UniqueEffect,
	UniqueEffect2 = UniqueEffect#unique_effect{attack_add=DamageAdd + Add},
	?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2});
trainer_special_enter_skill(3,Pos,Add,_) ->
	#unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
	#unique_effect{damage_dec=DamageDec} = UniqueEffect,
	UniqueEffect2 = UniqueEffect#unique_effect{damage_dec=DamageDec + Add},
	?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2});
trainer_special_enter_skill(4,Pos,Add,_) ->
	#unique_buffer{use=UniqueEffect} = UniqueBuffer = ?get_unique_buffer(Pos),
	#unique_effect{crit_hit_add=Crit_hit_add} = UniqueEffect,
	UniqueEffect2 = UniqueEffect#unique_effect{crit_hit_add=Crit_hit_add + Add},
	?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2}).

-define(state_normal,0).
-define(state_boom,1).

do_trainer_battle_skill(#arg_env{isChange=IsChange})->
    A1 = trainer_battle_skill(1),
    A2 = trainer_battle_skill(-1),
    if IsChange -> A2 ++ A1;
       true -> A1 ++ A2 
    end.

trainer_battle_skill(Type) ->
    case get({trainer_battle_speed, Type}) of
        {{Rou,Co,Speed}=Buf,History} ->
            {Q1,Q2} = lists:foldl(fun(#fighter_info{pos=Pos}=F,{A1,A2})->
                                          if Pos * Type > 0 ->{[F|A1],A2};
                                             true -> {A1,[F|A2]}
                                          end end, {[],[]}, ?get_queue()),
            Q11 = util:random_list(Q1),
            {RAction,History2} = lists:foldl(fun({Round, GPos,Dec},{RAAcc,HisAcc}) ->
                                                     if Round =< 1 -> 
                                                            #ger{gerSpeed=S1,gerHp=GerHp}=Ger = ?get_ger(GPos),
                                                            if GerHp > 0 ->
                                                                   Ger2 = Ger#ger{gerSpeed = S1+Dec},
                                                                   ?set_ger(Ger2),
                                                                   {[?action(277,GPos,[],Dec,0,?STATE_DEFAULT)|RAAcc],HisAcc};
                                                               true -> {RAAcc,[{Round - 1, GPos, Dec}|HisAcc]}
                                                            end;
                                                        true -> {RAAcc,HisAcc} 
                                                     end end, {[],[]}, History),
            {D1,D2,D3,Action,Action2,History3} = lists:foldl(fun(_,{B1,B2,QAcc,Act,Act2,HisAcc}) ->
                                                 case B2 of [] -> {B1,B2,QAcc,Act,Act2,HisAcc};
                                                     _ ->
                                                         %{B1m,B2m,QAccm,Actm,HisAccm} = 
                                                             do_dec_speed(B2,B1,QAcc,Act,Act2,HisAcc,Rou,Speed)
%%                                                      [H=#fighter_info{speed=S,pos=GPos}|T] ->
%%                                                          #ger{gerSpeed=S1}=Ger = ?get_ger(GPos),
%%                                                          Ger2 = Ger#ger{gerSpeed = S1-Speed},
%%                                                          ?set_ger(Ger2),
%%                                                          Act2 = [?action(275,GPos,[],-Speed,0,?STATE_TALENT),?action(276,GPos,[],0,0,?STATE_DEFAULT)|Act],
%%                                                          {[H#fighter_info{speed=S-Speed}|B1],T,Act2,[{Rou,GPos,Speed}|HisAcc]}
                                                 end 
                                         end, {[],Q11,[],[],[],History2}, lists:seq(1, Co)),
            ?set_queue(D1++D2++D3++Q2),
            put({trainer_battle_speed,Type},{Buf,History3}),
            Action2++Action++RAction;
        _-> []
    end.

    
do_dec_speed([],B1,QAcc,Act,Act2,HisAcc,_,_) -> {B1,[],QAcc,Act,Act2,HisAcc};
do_dec_speed([H=#fighter_info{speed=S,pos=GPos}|T],B1,QAcc,Act,Act2,HisAcc,Rou,Speed) ->
    #ger{gerSpeed=S1,gerHp=GerHp} = Ger = ?get_ger(GPos),
    if GerHp =< 0 -> do_dec_speed(T,B1,[H|QAcc],Act,Act2,HisAcc,Rou,Speed);
       true ->
           Ger2 = Ger#ger{gerSpeed = S1-Speed},
           ?set_ger(Ger2),
           Act00 = [?action(275,GPos,[],0,0,?STATE_DEFAULT)|Act],
           Act20 = [?action(276,GPos,[],0,0,?STATE_DEFAULT)|Act2],
           {[H#fighter_info{speed=S-Speed}|B1],T,QAcc,Act00,Act20,[{Rou,GPos,Speed}|HisAcc]}
    end.
    
do_trainer_action0(#arg_env{isChange=_IsChange, attTr={AttackerTrL,AttackerTrF},dfdTr={DefenderTrL,DefenderTrF}}=Env,Count,ActionList) ->
    Count2 = (Count + 1) rem 2, 
    RmvAction = lists:foldl(fun(#fighter_info{pos=Pos},Acc) when abs(Pos) < 10 ->check_tr_duration(Pos)++Acc ;(_,Acc) -> Acc end, [], ?get_alives()),
    {AtkSpeed,DfdSpeed} = get_trainer_speed(),
    %TrainerList = if IsChange -> [{-1,DefenderTrL},{1,AttackerTrL},{-2,DefenderTrF},{2,AttackerTrF}];
    %                true -> [{1,AttackerTrL},{-1,DefenderTrL},{2,AttackerTrF},{-2,DefenderTrF}] 
    %             end,
    TrainerList = lists:keysort(3,[{-1,DefenderTrL,DfdSpeed},{1,AttackerTrL,AtkSpeed},{-2,DefenderTrF,DfdSpeed},{2,AttackerTrF,AtkSpeed}]),
    case do_trainer_action0(lists:reverse(TrainerList),[],Count2,Env,RmvAction++ActionList) of
        {PosTrList,ActionList2} ->
            {Tr1,Tr2,Tr3,Tr4} = get_pos_tr_data(PosTrList),
                {ActionList2,Env#arg_env{attTr={Tr1,Tr3},dfdTr={Tr2,Tr4}}};
        %{[Tr4,Tr3,Tr2,Tr1],ActionList2} -> 
        %   if IsChange ->
        %           {ActionList2,Env#arg_env{attTr={Tr2,Tr4},dfdTr={Tr1,Tr3}}};
        %      true ->
        %           {ActionList2,Env#arg_env{attTr={Tr1,Tr3},dfdTr={Tr2,Tr4}}}
        %   end;
        _ ->
            ignore
    end.

get_pos_tr_data(List)-> get_pos_tr_data(List,{0,0,0,0}).
get_pos_tr_data([],X)->X;
get_pos_tr_data([{1,V}|T],{_,B,C,D})-> get_pos_tr_data(T,{V,B,C,D});
get_pos_tr_data([{2,V}|T],{A,B,_,D})-> get_pos_tr_data(T,{A,B,V,D});
get_pos_tr_data([{-1,V}|T],{A,_,C,D})->get_pos_tr_data(T,{A,V,C,D});
get_pos_tr_data([{-2,V}|T],{A,B,C,_})->get_pos_tr_data(T,{A,B,C,V}).

do_trainer_action0([],Trx,_Count,#arg_env{isChange=IsChange}=Env,ActionList) ->
	case check_win(IsChange) of
		{ok,Result} ->
			fight_over(ActionList,Result,Env);
		_ ->
			{Trx,ActionList}
	end;
do_trainer_action0([{Pos,Tr,_}|OtherTr],Trx,Count,#arg_env{isChange=IsChange}=Env,ActionList)->
	{Tr2,Actions2} = do_trainer_action(Tr,Pos,Count),
	case check_win(IsChange) of
		{ok,Result} ->
			fight_over(Actions2++ActionList,Result,Env);
		_ ->
			do_trainer_action0(OtherTr,[{Pos,Tr2}|Trx],Count,Env,Actions2++ActionList)
	end.

get_trainer_speed() ->
	lists:foldl(fun(#fighter_info{speed=Speed,pos=Pos},{AAcc,DAcc}) ->
						if Pos > 0 -> {AAcc+Speed,DAcc};
						   true -> {AAcc,DAcc+Speed}
						end end	, {0,0}, ?get_queue()).
do_trainer_action(#trSpecial{specialID=0}=Tr,T,_) ->
        A1 = trainer_battle_skill(T),
	{Tr,A1};
do_trainer_action(#trSpecial{roleLevel=Level,specialID=TrSpecialID}=Tr,T,Count) ->
	%%先获取出皮肤带来的属性变化
	ASkinBuff = get_skinbuff(T),
	SkinTrainerBuff = role_skin:get_skinbuff_for_trainer(ASkinBuff),
	{Tr2,SAction0} = change_state(Tr,T,0),
	Skill = calc_trSkill(Tr2,Count),
	Tr3 = calc_newTr(Tr2),
	SpecialIDList = data_trSpecial:get({specialID,TrSpecialID}),
	SkillInfo = data_trSpecial:get({skill,Skill}),
	#trBuff{type=SkillType,special=Special,target=TarSelect}=SkillInfo,
	TarPosList1 = select_target(TarSelect,T,true),
	TarPosList = case lists:member(Skill,data_trSpecial:get(data_mark_skillIDList)) of
		true->
			%%此处根据玩家皮肤的属性增加标记的目标数量			
			EnemyList = enemy_alive(T),
			FindTarGerList = random_select_list(SkinTrainerBuff#skin_buff.trainer_mark_plus,EnemyList--TarPosList1),
			FindTarGerList++TarPosList1;
		false->
			TarPosList1
	end,
	TarPosList2 = 
	case Special of
		2 ->
			TarPosList;
		1 ->
			L1 = [begin 
					  #ger{gerBase=#gerBase{gerTypeID=TypeID}} = ?get_ger(Pos),
					  #data_ger{gerProperty=ID0} = data_ger:get(TypeID),
					  {Pos,ID0} 
				  end ||#fighter_info{pos=Pos}<-?get_alives(), abs(Pos) < 10],
			[Pos||{Pos,ID0}<-L1,lists:member(ID0,tuple_to_list(SpecialIDList))];
		3 ->
			TarPosList
	end,
	SpDiff = Tr3#trSpecial.sp - Tr2#trSpecial.sp,
	Action3 = apply_trSkill({SkillType,Skill},SkillInfo,TarPosList2,Level,T,TrSpecialID,SpDiff,role_skin:get_skinbuff_for_trainer_even(SkinTrainerBuff,Count)),
	{Tr4,SAction1} = change_state(Tr3,T,1),
        A1 = trainer_battle_skill(T),
	{Tr4,A1 ++ SAction1++Action3++SAction0}.

apply_trSkill({5,_},SkillInfo,TarPosList,Level,T,TrSpecialID,SpDiff,_SkinBuff) ->
	#trBuff{modulusBase=Base,modulusCo=Co,duration=Duration}=SkillInfo,
	Value = Base+Level*Co,
	Actions = lists:foldl(fun(Pos,Acc)->
								  Acc0 =apply_trSkill_effect(Pos,Duration,Value,0,5,TrSpecialID),
								  Acc0 ++ Acc
						  end, [], TarPosList),
	TL = [Pos||Pos<-TarPosList],
	Actions2 = apply_trSkill_effect(Actions),
	Actions2 ++[?action(?ACTION_TR_RIP,T,TL,0,SpDiff,?STATE_TRAINER)];
apply_trSkill({6,Skill},SkillInfo,TarPosList,Level,T,_TrSpecialID,SpDiff,_SkinBuff) ->
	#trBuff{modulusBase=Base,modulusCo=Co,buff=Buff}=SkillInfo,
	Value = calc_cur_value(Base+Level*Co),
	{TL,Actions} = 
		lists:foldl(fun(Pos,{PosAcc,ActionAcc}) ->
							#unique_buffer{use=TarUniqueBuffer} = ?get_unique_buffer(Pos),
							#unique_effect{invinciblep=TarInvinciblep,spRa=SpRa} = TarUniqueBuffer,
							case TarInvinciblep of
								false ->
									Ger=?get_ger(Pos),
									MaxHp = ?a(Ger,gerHpMax),
									AddHp = trunc(MaxHp * Value / 10000),
									Hp2 = add_hp(Ger#ger.gerHp,MaxHp,AddHp),
									Sp2 = trunc(real_add_sp(Ger#ger.gerSp,Buff,(Ger#ger.gerAttr)#gerAttr.gerSpMax) * SpRa/100),
									Sp3 = Ger#ger.gerSp + Sp2,
									Ger2 = Ger#ger{gerSp=Sp3,gerHp=Hp2},
									
									?set_ger(Ger2),
									AActions = [?action(101,Pos,[],AddHp,0,?STATE_TALENT),?action(113,Pos,[],Buff,0,?STATE_TALENT)],
									{[Pos|PosAcc],AActions++ActionAcc};
								_ ->
									AActions = [?action(101,Pos,[],0,0,?STATE_TALENT),?action(113,Pos,[],0,0,?STATE_TALENT)],
									{[Pos|PosAcc],AActions++ActionAcc}
							end
					end,{[],[]},TarPosList),
	ActID = case Skill of 6 -> ?ACTION_TR_COOL; 10 -> ?ACTION_TR_COOL_ALL end,			
	Actions++[?action(ActID,T,TL,0,SpDiff,?STATE_TRAINER)];
apply_trSkill({7,Skill},SkillInfo,TarPosList,Level,T,TrSpecialID,SpDiff,_SkinBuff) ->
	#trBuff{modulusBase=Base,duration=Duration,modulusCo=Co,buff=Buff}=SkillInfo,
	Value = Base+Level*Co,
	TL = [Pos||Pos<-TarPosList],
	Actions = lists:foldl(fun(Pos,Acc)->
								  Acc0 =apply_trSkill_effect(Pos,Duration,Value,Buff,7,TrSpecialID),
								  Acc0 ++ Acc
						  end, [], TarPosList),
	ActID = case Skill of 7 -> ?ACTION_TR_THIRTY; 11 -> ?ACTION_TR_THIRTY_ALL end,
	Actions2 = apply_trSkill_effect(Actions),
	Actions2 ++[?action(ActID,T,TL,0,SpDiff,?STATE_TRAINER)];
apply_trSkill({8,Skill},SkillInfo,TarPosList,Level,T,TrSpecialID,SpDiff,_SkinBuff) ->
	#trBuff{modulusBase=Base,duration=Duration,modulusCo=Co,buff=Buff}=SkillInfo,
	Value = Base+Level*Co,
	TL = [Pos||Pos<-TarPosList],
	Actions = lists:foldl(fun(Pos,Acc)->
								  Acc0 =apply_trSkill_effect(Pos,Duration,Value,Buff,8,TrSpecialID),
								  Acc0 ++ Acc
						  end, [], TarPosList),
	ActID = case Skill of 8 -> ?ACTION_TR_STONE ; 12 -> ?ACTION_TR_STONE_ALL end,
	Actions2 = apply_trSkill_effect(Actions),
	Actions2++[?action(ActID,T,TL,0,SpDiff,?STATE_TRAINER)];
apply_trSkill({9,Skill},_SkillInfo,TarPosList,_Level,T,TrSpecialID,SpDiff,_SkinBuff) ->
	TL = [Pos||Pos<-TarPosList],
	Actions = lists:foldl(fun(Pos,Acc)->
								  Acc0 =apply_trSkill_effect(Pos,1,0,0,9,TrSpecialID),
								  Acc0 ++ Acc
						  end, [], TarPosList),
	ActID = case Skill of 9 -> ?ACTION_TR_KNOW; 13 -> ?ACTION_TR_KNOW_ALL end,
	Actions2 = apply_trSkill_effect(Actions),
	Actions2 ++[?action(ActID,T,TL,0,SpDiff,?STATE_TRAINER)];
apply_trSkill({14,_},SkillInfo,TarPosList,Level,T,_TrSpecialID,SpDiff,SkinBuff) ->
	#trBuff{modulusBase=Base}=SkillInfo,
	%%此处增加了皮肤增加的伤害值
	Damage = trunc(calc_damage_ratio(Level) * Base),
	HAction = [?action(?ACTION_TR_SNOW3,TarGerPos,[],0,0,?STATE_DEFAULT)||TarGerPos<-TarPosList],
	{Actions,TL,RebornActions} =
		lists:foldl(fun(Pos,{Acc1,Acc2,RebornAcc})->
							Ger = ?get_ger(Pos),
							{_Ger2,ActionList,RebornAction} = apply_trSkill(Ger,Damage,?ACTION_TR_SNOW3,SkinBuff),
							{ActionList++Acc1,[Pos|Acc2],RebornAction++RebornAcc}
					end,{HAction,[],[]},TarPosList),
	RebornActions ++ Actions ++[?action(?ACTION_TR_SNOW2,T,TL,0,SpDiff,?STATE_TRAINER)
				,?action(?ACTION_TR_SNOW,T,TL,0,0,?STATE_TRAINER)];
apply_trSkill({15,_},SkillInfo,TarPosList,Level,T,_TrSpecialID,SpDiff,SkinBuff) ->
	#trBuff{modulusBase=Base}=SkillInfo,
	Damage = erlang:trunc(calc_damage_ratio(Level) * Base*(1+SkinBuff#skin_buff.even_trainer_demage_plus/10000)),
	Len = max(length(TarPosList),1),
	Damage2 = trunc(Damage / Len),
	HAction = [?action(?ACTION_TR_AREOLITE3,TarGerPos,[],0,0,?STATE_DEFAULT)||TarGerPos<-TarPosList],
	{Actions,TL,RebornActions} =
		lists:foldl(fun(Pos,{Acc1,Acc2,RebornAcc})->
							Ger = ?get_ger(Pos),
							{_Ger2,ActionList,RebornAction} = apply_trSkill(Ger,Damage2,?ACTION_TR_AREOLITE3,SkinBuff),
							{ActionList++Acc1,[Pos|Acc2],RebornAction++RebornAcc}
					end,{HAction,[],[]},TarPosList),
	RebornActions ++ Actions ++ [?action(?ACTION_TR_AEROLITE2,T,TL,0,SpDiff,?STATE_TRAINER)
				 ,?action(?ACTION_TR_AEROLITE,T,TL,0,0,?STATE_TRAINER)];
apply_trSkill({16,_},SkillInfo,TarPosList,Level,T,_TrSpecialID,SpDiff,SkinBuff) ->
	#trBuff{modulusBase=Base}=SkillInfo,
	Damage = trunc(calc_damage_ratio(Level) * Base),
	HAction = [?action(?ACTION_TR_KILL3,TarGerPos,[],0,0,?STATE_DEFAULT)||TarGerPos<-TarPosList],
	{Actions,TL,RebornActions} =
		lists:foldl(fun(Pos,{Acc1,Acc2,RebornAcc})->
							Ger = ?get_ger(Pos),
							{_Ger2,ActionList,RebornAction} = apply_trSkill(Ger,Damage,?ACTION_TR_KILL3,SkinBuff),
							{ActionList++Acc1,[Pos|Acc2],RebornAction++RebornAcc}
					end,{HAction,[],[]},TarPosList),
	RebornActions ++ Actions ++ [?action(?ACTION_TR_KILL2,T,TL,0,SpDiff,?STATE_TRAINER)
				 ,?action(?ACTION_TR_KILL,T,TL,0,0,?STATE_TRAINER)];
apply_trSkill({17,_},SkillInfo,TarPosList,Level,T,_TrSpecialID,SpDiff,SkinBuff) ->
	#trBuff{modulusBase=Base,duration=Duration}=SkillInfo,
	Damage = trunc(calc_damage_ratio(Level) * Base),
	{Actions1,Actions2} = add_trSkill_dot_buffer(TarPosList,Damage,Duration,?ACTION_TR_HURT3,114,SkinBuff),
	Actions1 ++ Actions2 ++[?action(?ACTION_TR_HURT2,T,TarPosList,0,SpDiff,?STATE_TRAINER)
				,?action(?ACTION_TR_HURT,T,TarPosList,0,0,?STATE_TRAINER)].

apply_trSkill(TarGer,Damage,ActionID,SkinBuff) ->
    #ger{gerBase=#gerBase{gerPos=TarGerPos},gerAttr=TarAttr,gerHp=TarHp,gerSp=TarSp,gerProHp=TarPropHp}=TarGer,
    #unique_buffer{use=TarUniqueBuffer} = ?get_unique_buffer(TarGerPos),
    #unique_effect{damage_plus={TarDamagePlus,_,_},damage_sub={TarDamageSub,_},invinciblep=TarInvinciblep,damage_dec=DamageDec1,spRa=SpRa} = TarUniqueBuffer,
    DamageDec = case lists:member(ActionID,[?ACTION_TR_SNOW3,?ACTION_TR_AREOLITE3,?ACTION_TR_KILL3]) of
    	false->
    		DamageDec1,
    		SkinBuff=#skin_buff{};
    		%TarGerSkinBuff=#skin_buff{};
    	true->
    		TarGerSkinBuff = ?get_pos_skinbuff(TarGerPos),
    		DamageDec1-SkinBuff#skin_buff.even_trainer_demage_plus+TarGerSkinBuff#skin_buff.ger_demage_sub
    end,
	%% 无敌的精灵不会受到伤害,也不会反弹伤害(如果也不触发暴击,这把这个判断前移)
	case TarInvinciblep of
		false ->
			HarmValueT0 = erlang:trunc(Damage*(1 + (TarDamagePlus - TarDamageSub) / 100 - DamageDec / 10000)),
			{Inv0,HarmValue} = check_d_minHp(TarGerPos,HarmValueT0),
			InvAction = if Inv0 -> set_d_inv(); true -> [] end,
            TarAddSp = trunc(real_add_sp(TarSp,?HURT_ADD_SP,(TarGer#ger.gerAttr)#gerAttr.gerSpMax)* SpRa/100),
            TarSp2 = TarSp + TarAddSp,
            {ProHpDecf,HpDecf} = decf_ger_hp(TarPropHp,HarmValue),
        	TarHp2 = add_hp(TarHp,TarAttr#gerAttr.gerHpMax,HpDecf),
            TarProHp2 = TarPropHp + ProHpDecf,
			TarGerT = TarGer#ger{gerSp=TarSp2,gerHp=TarHp2,gerProHp=TarProHp2},
			?set_ger(TarGerT),
			State = 
				case TarHp2 =< 0 of
					true ->
						{IsReborn,RebornActionT,_} = do_reborn(0,TarGerPos,TarGerT,{0,[],[]}),
						case IsReborn of
							false ->
								RebornAction = stand_fellow(TarGerPos)++RebornActionT ,
								?STATE_DEAD;
							_ ->
								RebornAction = RebornActionT,
								?STATE_DEFAULT
						end;
					_ ->
						RebornAction = [],
						?STATE_DEFAULT
				end,
			TarGer2 = ?get_ger(TarGerPos);
		_ ->
			InvAction = [],
			RebornAction = [],
			State = ?STATE_DEFAULT,
			TarAddSp = 0,
			HpDecf=0,
			ProHpDecf=0,
			HarmValue = 0 ,
			TarGer2=TarGer
	end,
	Action = [?pro_action(?ACTION_NORMAL_HURT,TarGerPos,[],HpDecf,ProHpDecf,TarAddSp,State)],
	#attack_info{action=LinkAction}=do_damage_link(0,TarGerPos,#attack_info{damage=HarmValue}),
	{TarGer2,Action++LinkAction++InvAction,RebornAction}.

calc_damage_ratio(Level) ->
	if Level > 200 -> {X1,X2} = data_trSpecial:get({damage_ratio,200}), 100 * ( math:pow(2,(X1 + (Level - 200) * X2 /100)));
	   Level > 100 -> {X1,X2} = data_trSpecial:get({damage_ratio,100}), 100 * ( math:pow(2,(X1 + (Level - 100) * X2 /100)));
	   true -> X1 = data_trSpecial:get({damage_ratio,0}),100 * ( math:pow(2, Level * X1 / 100))
	end.

calc_newTr(#trSpecial{sp=Sp,state=State}=Tr) ->
	case State of
		?state_normal ->
			Tr#trSpecial{sp=Sp+2};
		?state_boom ->
			Tr#trSpecial{sp=Sp-2}
	end.

calc_trSkill(#trSpecial{specialID=ID,state=State},Count) ->
	case State of
		?state_normal ->
			case Count of
				1 ->
					data_trSpecial:get({skill2,ID});
				0 ->
					data_trSpecial:get({skill3,ID})
			end;
		?state_boom ->
			case Count of
				1 ->
					data_trSpecial:get({skill4,ID});
				0 ->
					data_trSpecial:get({skill5,ID})
			end
	end.

change_state(#trSpecial{sp=Sp,state=State}=Tr,_T,0) ->
	case State of
		?state_normal ->
			if Sp < 4 ->
				   {Tr,[]};
			   true ->
			   		%%此处取消变身，只是修改返回的变身action为空（v3.3.0）
				   % {Tr#trSpecial{state=?state_boom},[?action(125,T,[],0,0,?STATE_TRAINER)]}
				   {Tr#trSpecial{state=?state_boom},[]}
			end;
		_ ->
			{Tr,[]}
	end;
change_state(#trSpecial{sp=Sp,state=State}=Tr,_T,1) ->
		case State of
			?state_boom ->
			if Sp < 2 ->
				   {Tr#trSpecial{state=?state_normal},[]};
				   %%此处取消变身，只是修改返回的变身action为空（v3.3.0）
				   % {Tr#trSpecial{state=?state_normal},[?action(126,T,[],0,0,?STATE_TRAINER)]};
			   true ->
				   {Tr,[]}
			end;
			_ ->
				{Tr,[]}
	end.
	
append_enter_skill_attr(Tar,AddAttr)                                                                                                          ->
	#ger{gerExtra=GerExtra} = Tar,
    AddAttr2 = 
	    case is_record(GerExtra,add_attr) of
		    true ->
                ger_attr:append_add_attr(GerExtra,AddAttr);
		    false ->
                AddAttr
	    end,
	Tar#ger{gerExtra=AddAttr2}.

rm_tar_maimed_action(SrcGerPos,CheckType) ->
    OldMaimedList = ?get_target_maimed(SrcGerPos),
    {NewIDList, NewActionList} = 
        lists:foldl(fun(E, {NIDL, NAL}) ->
                        CI = #maimed{tarpos=TarPos,srcpos=SrcPos, duration=Duration, type=Type, effectId=EffectID} = ?get_maimed(E),
						NewDuration =  case CheckType of 1 -> Duration - 1;
							2 -> Duration - 0.5
						end,
						%%  回合后判断解除,不接触的数据不变.
                        if NewDuration  < 0.1 ->
                                %% 清除被施加方的眩晕标记(TODO 之后可以改用unique_buff记录)
                                ?delete_maimed(E),
                                ?delete_ger_maimed(TarPos, E),
                                ?delete_ger_other_maimed(TarPos, E),
                                %% 新的列表+消除动作
								if EffectID == 0  ->
									   {DisActionID, State} = get_display_id_and_state(Type),
									   Action = ?action(DisActionID,TarPos,[],0,0,State),
									   {NIDL, [Action|NAL]};
								   Type == ?ACTION_UNSLEEP orelse Type == 240 ->
									   {DisActionID, State} = get_display_id_and_state(Type),
									   UniqueBuffer = #unique_buffer{effects=Effects} = ?get_unique_buffer(TarPos), 
									   Effects2 = [E0||#effect_record{effectId=EEFTID}=E0<-Effects,EEFTID /= EffectID],
									   ?set_unique_buffer(TarPos, UniqueBuffer#unique_buffer{effects=Effects2}),
									   Action = ?action(DisActionID,TarPos,[],0,0,State),
									   {NIDL, [Action|NAL]};
								   true ->
									   %% 这里目前只有链接和狐狸的技能,旧的绝技暂时不处理 
									   UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(TarPos), 
									   {UseEffect2, Action} = sub_unique_effect(TarPos,UseEffect,EffectID,Type,[SrcPos]),
									   ?set_unique_buffer(TarPos, UniqueBuffer#unique_buffer{use=UseEffect2}),
									   {NIDL, Action ++ NAL}
								end;
                            true ->
								case CheckType of 1 -> ?set_maimed(E, CI#maimed{duration=NewDuration});
									2 -> ignore
								end,
                                {[E|NIDL], NAL}
                        end 
                    end, {[], []}, OldMaimedList),
    ?set_target_maimed(SrcGerPos, NewIDList),
    NewActionList.

fight_loop([#fighter_info{pos=SrcGerPos}=FighterBase|Tail],Count,ActionList,#arg_env{isChange=IsChange}=ArgEnv) ->
    #attack_info{action=ActionList2T} = check_dot(SrcGerPos,#attack_info{action=ActionList}),
    %% 因为buff计数挂在施加方身上的,所以不管其死没死,都要进行buff计数
	MaimedAction = rm_tar_maimed_action(SrcGerPos,1),
    ActionList2 = MaimedAction ++ ActionList2T,
    Alives = ?get_alives(),
    
    %% 每回合开始前的操作可能已经杀死了所有的精灵, 所以这里需要进行结束判断
    case check_win(Alives, IsChange) of
        {ok, Result} ->
            fight_over(ActionList2,Result,ArgEnv);
        _ ->
			?set_loop_queue(Tail),
            %% 判断当前的精灵死了没
            case lists:keyfind(SrcGerPos,#fighter_info.pos,Alives) of
                false -> 
        	        fight_loop(Tail,Count,ActionList2,ArgEnv);
                _ ->
                    SrcGer = ?get_ger(SrcGerPos),
                    case is_need_skip_op(SrcGerPos) of
                        false ->
                			%% 判断是否已死亡(不应该出现这种情况)
                			if SrcGer#ger.gerHp =< 0 ->
                                    %%?ERR("有些精灵:~p,它已经死了,但还活在训练师心中.~n",[SrcGerPos]),
        	                        fight_loop(Tail,Count,ActionList2,ArgEnv);
                			   true ->
								   UniqueHook = ?get_unique_hook(SrcGerPos),
								   AtkInfo0 = #attack_info{uaction=UAction}=before_any_action_hook(UniqueHook,SrcGerPos,#attack_info{}),
								   gen_cp_fighter(SrcGerPos,FighterBase),
								   #fighter_info{skill=Skills} = lists:keyfind(SrcGerPos,#fighter_info.pos,?get_alives()),
        						   clean_miss_data(),
                				   %% 选择技能
                				   {Type,DataSkill} = select_skill(SrcGer,Skills),
                
                				   %% 计算目标选择
                				   %#data_skill{targetSelect=TarSelect}=DataSkill,
                				   TarPosList = select_target(DataSkill,SrcGerPos),

								   %% 攻击前的hook
                                   ActionList3 = before_attack_hook(SrcGerPos,UAction++ActionList2),

                				   %% 进行攻击
                                    #unique_buffer{use=#unique_effect{maxCount=MaxCC}} = ?get_unique_buffer(SrcGerPos),
                                   AtkInfo = AtkInfo0#attack_info{uaction=[],action=ActionList3,type=Type,maxCount=MaxCC+1},
                				   case apply_attack(Skills,IsChange,DataSkill,SrcGerPos,TarPosList,AtkInfo) of
                                        {ok,Result,AttackActionList} ->
                                            fight_over(AttackActionList,Result,ArgEnv); 
                                        AtkInfo2 ->
                                           %% 攻击后的hook
                                           
                                           #attack_info{action=AUAHActionList, uaction=Uaction} = after_unique_attack_hook(?get_unique_hook(SrcGerPos), SrcGerPos, TarPosList, AtkInfo2),
                                           AttackActionList = Uaction ++ AUAHActionList,
                                           
                                           AttackActionList2 = after_attack_hook(SrcGerPos,AttackActionList),
                                           AttackActionList3 = ger_change_status(SrcGerPos, Type) ++ AttackActionList2,

                						   #gerSkill{god=GodSkills}=Skills,
                						   {GodTimes,_} = ?get_god_skill(SrcGerPos),
                						   if GodSkills =/= [] andalso GodTimes < 5 ->
                								  add_god_attr(SrcGerPos,GodSkills);
                							  true -> 
                								  ignore
                						   end,
                                            %% 更新动态绝技有效期,这个只能放在战斗后做,之前在战斗前做,然后cache下来,战斗后更新,结果把战斗中更新
                                            %% 的数据给抹掉了
                                            {NewUniqueBuffer, RmvActions} = check_unique_duration(SrcGerPos),
                                            ?set_unique_buffer(SrcGerPos, NewUniqueBuffer),
        	                                fight_loop(?get_loop_queue(),Count,RmvActions ++ AttackActionList3,ArgEnv) 
                                   end 
                            end;
                        _ ->
                            {NewUniqueBuffer, RmvActions} = check_unique_duration(SrcGerPos),
                            ?set_unique_buffer(SrcGerPos, NewUniqueBuffer),
							MaimedAction2 = rm_tar_maimed_action(SrcGerPos,2),
							fight_loop(?get_loop_queue(),Count,MaimedAction2 ++ RmvActions ++ ActionList2,ArgEnv) 
                	end 
            end
    end;

%% 重头开始依次发动攻击
fight_loop([],Count,ActionListT,#arg_env{roleID=RoleID,isChange=IsChange}=ArgEnv)                                   ->
	case check_win(?get_alives(), IsChange) of
		{ok, Result} ->
			fight_over(ActionListT,Result,ArgEnv);
		_ ->
			%% 计算是否超回合
			NewCount = Count+1,
			MaxCount = 
				case RoleID =:= 0 of
					false ->
						data_fight:get(max_pve_fight_loop);
					_ ->
						data_fight:get(max_pvp_fight_loop)
				end,
			case NewCount >= MaxCount of
				true ->
					fight_over(ActionListT,false,ArgEnv);
				false ->
					%% 客户端通过这个判断每回合开始
					ActionList = [?action(?ACTION_LOOP_START,0,[],0,0,?STATE_DEFAULT)|ActionListT],
					case do_trainer_action0(ArgEnv,NewCount,ActionList) of
						{ActionList2,ArgEnv2} ->
							fight_loop(sort_fighters(?get_queue()),NewCount,new_loop_hook(ActionList2),ArgEnv2);
						_ ->
							ignore
					end
			end
	end.

%% 释放god技能动作
add_god_attr(GerPos,GodSkills)                                                                                                                ->
	#ger{gerSp=GerSp,gerHp=GerHp,gerProHp=GerProHp,gerAttr=BaseAttr,gerSpeed=GerSpeed} = Ger = ?get_ger(GerPos),
	%% InitGer 为初始状态 + 已统计过的addattr(addattr为gerExtra字段),计算时，将当前需增加的值和addattr合并，重新运算属性
	%% 属性运算完成后，将属性与初始状态一同使用登场计算获取武将各项属性值，
	%% 更新新武将的hp、sp、proHp为重算属性前上阵武将的hp、sp、proHp
	{_,InitGer} = ?get_god_skill(GerPos), 
	{NewGer,InitGer3} = lists:foldl(fun(SkillID,{GerAcc,InitGerAcc})->
												case data_skill:get(SkillID) of
													#data_skill{gerEnterOrGodAdd=SkillAdd}->
														AddAttr2 = case ?CATCH(ger_attr:append_add_attr_no_Anti(InitGerAcc#ger.gerExtra,SkillAdd)) of
																	   {'EXIT',_}=L->
																		   ?ERR("calc god wrong :GerAcc=~w,InitGerAcc=~w \n SkillID=~w,SkillID=~w,Info=~w",
																				[GerAcc,InitGerAcc,SkillID,SkillAdd,L]),
																		   L;
																	   L ->
																		   L
																   end,
										   {GerAcc#ger{gerExtra=AddAttr2},InitGerAcc#ger{gerExtra=AddAttr2}};
									   _ ->
										   {GerAcc,InitGerAcc}
								   end
						   end,{Ger,InitGer},[random_one_skill(GodSkills)]),
	%% 基础属性重算
	#ger{gerAttr=#gerAttr{gerAttack=NGerAttack}}=NewGer2 = ger_attr:add_enter_skill_attr(InitGer,NewGer#ger.gerExtra),
	%% sp、hp、proHp值置为当前值
	NewGer3 = NewGer2#ger{gerSp=GerSp,gerHp=GerHp,gerProHp=GerProHp,gerAttr=BaseAttr#gerAttr{gerAttack=NGerAttack},gerSpeed=GerSpeed},
	record_ger_god_skill(GerPos,InitGer3),
	
	?set_ger(NewGer3).

record_ger_god_skill(GerPos,Ger)                                                                                                              ->
	{N,_} = ?get_god_skill(GerPos),
	erlang:put({god,GerPos},{N + 1,Ger}).

%% 复活动作, SrcPos: 攻击方 GerPos:被攻击死亡方
do_reborn(SrcPos,GerPos,Acc) ->
    Ger = ?get_ger(GerPos),
    do_reborn(SrcPos, GerPos, Ger, Acc).

do_reborn(SrcPos,GerPos,Ger,{_,ActionAcc,KillHookAcc}=Acc)                                                                         ->
    Alives = ?get_alives(),
	if Ger#ger.gerHp =< 0 ->
           %% 现在有链接和反伤等,有可能某处判断时,对应位置的精灵已经被移除了
           case lists:keytake(GerPos,#fighter_info.pos,Alives) of
                false ->
                    Acc;
		       {value,#fighter_info{skill=#gerSkill{god2=God2Skill}},_Alives2} ->
        		   IsReborn = ?is_reborn(GerPos),
                   ClearBuffAction = clear_buffer(GerPos),
        		   if God2Skill =:= [] orelse IsReborn ->
						  %% 需求要击杀做特殊处理,将多个action合并成一个
						  NewKillHookAcc = 
							  lists:foldl(fun(#p_action{actionID=ActionID,addHp=Value}=Action,KillAcc) ->
												  case lists:keytake(ActionID,#p_action.actionID,KillHookAcc) of
													  false ->
														  [Action|KillAcc];
													  {value,#p_action{addHp=OldValue}=OldAction,KillAcc2} ->
														  [OldAction#p_action{addHp=Value + OldValue}|KillAcc2]
												  end 
										  end,KillHookAcc,killed_hook(SrcPos,GerPos)),
                          DeadAction = ?action(?ACTION_DEAD,GerPos,[],0,0,?STATE_DEFAULT),
						  NewActionAcc = dead_hook(GerPos,SrcPos,[DeadAction|ActionAcc]),
						  ?set_ger(Ger#ger{gerHp=0}),
                          ?delete_alive(GerPos),
						  delete_damage_link(1,GerPos),
						  delete_damage_link(2,GerPos),
						  {false,ClearBuffAction++NewActionAcc,NewKillHookAcc};
        			  true ->
                          #data_skill{gerEnterOrGodAdd=OldAddAttr} = data_skill:get(random_one_skill(God2Skill)),
                          AddAttr = ger_attr:transOldAddAttr2NewAddAttr(OldAddAttr),
                          #add_attr{gerHpMaxAddtion=X} = AddAttr,
        				  Add = erlang:trunc(Ger#ger.gerAttr#gerAttr.gerHpMax * X / 10000),
        				  ActionReborn = ?action(?ACTION_REBORN,?b(Ger,gerPos),[],Add,0,?STATE_DEFAULT),% ActionID
                          NewActionAcc = reborn_hook(GerPos,[ActionReborn|ActionAcc]),
        				  Ger2 = Ger#ger{gerHp=Add},
        				  ?set_reborn(GerPos),
        				  ?set_ger(Ger2),
                          {true,ClearBuffAction++NewActionAcc,KillHookAcc}
        		   end 
            end;
	   true -> 
		   Acc 
	end.

%% 结束结算
fight_over(ActionList,Result,#arg_env{isChange=IsChange,fighterList=FighterList,from=From,ref=Ref})                             ->
	FighterList2 = [PGer#p_fighter{damage=get_damage_cc(PGerID)}||#p_fighter{gerID=PGerID}=PGer<-FighterList],
	FightRecord = #sc_fight_double_request{fighterList=FighterList2,actionList=ActionList,result=Result},
	%% 获取剩余血量和怒气
	{SrcGerStateList,DGerStateList,SrcGerList,DGerList,_DDeadList,RebornList1,RebornList2} = 
    lists:foldl(%fun(#fighter_info{pos=Pos},{Acc1,Acc2,Acc3,Acc4,Acc5}) ->
                        %#ger{gerID=GerID,gerHp=GerHp,gerSp=GerSp} = Ger = ?get_ger(Pos),
					fun({Pos,#ger{gerID=GerID,gerHp=GerHp,gerSp=GerSp,gerBase=#gerBase{gerTypeID=TGerTypeID}}=Ger}
					   ,{Acc1,Acc2,Acc3,Acc4,Acc5,Acc6,Acc7})->
                        GerState = {GerID,GerHp,GerSp},
                        case IsChange xor (Pos > 0) of 
                            true ->
                                case GerHp > 0 of
                                    true->
										case ?is_reborn(Pos) of
											true ->
												{[GerState|Acc1],Acc2,[Ger|Acc3],Acc4,Acc5,[{abs(Pos),GerID}|Acc6],Acc7};
											_ ->
												{[GerState|Acc1],Acc2,[Ger|Acc3],Acc4,Acc5,Acc6,Acc7}
										end;
                                    false->
                                        {[GerState|Acc1],Acc2,[Ger|Acc3],Acc4,Acc5,Acc6,Acc7}
                                end;
                            false ->
                                case GerHp > 0 of
                                    true->
										case ?is_reborn(Pos) of
											true ->
												{Acc1,[GerState|Acc2],Acc3,[Ger|Acc4],Acc5,Acc6,[{abs(Pos),GerID}|Acc7]};
											_ ->
												{Acc1,[GerState|Acc2],Acc3,[Ger|Acc4],Acc5,Acc6,Acc7}
										end;	
                                    false->
                                        {Acc1,[GerState|Acc2],Acc3,[Ger|Acc4],add_born_list(TGerTypeID,Acc5),Acc6,Acc7}
                                end
                        end;
				   (_,Accs) ->
						Accs
                end,{[],[],[],[],[],[],[]},get()),
	From ! {fight_result,Ref,{Result,FightRecord,{SrcGerStateList,DGerStateList,SrcGerList,DGerList,RebornList1,RebornList2}}},
    fight_over.

trainers2p_fighter({LeaderTr,FellowTr},Pos,{SkinInfo,SkinInfo2}) ->
	trainer2p_fighter(LeaderTr,Pos,SkinInfo)++trainer2p_fighter(FellowTr,Pos*(abs(Pos)+1),SkinInfo2).
trainer2p_fighter(#trSpecial{trID=0},_,_SkinInfo) ->
	[];
trainer2p_fighter(#trSpecial{trID=TrID,specialID=SpecialID,sp=Sp,roleLevel=RoleLevel},Pos,#skin_info{equip=Equip}) ->
    Transmigration = if
                         RoleLevel > 300 ->
                             1;
                         true ->
                             0
                     end,
    ?INFO("trainer2p_fighter ~w > ~w",[RoleLevel,Transmigration]),
	[#p_fighter{gerID=(TrID bsl 32) bor SpecialID
			  %% 此处应该修改训练师gerTypeID
			  ,gerTypeID=0
			  ,gerPos=Pos
			  ,gerHp=0
			  ,gerHpMax=Equip
			  ,gerProHp=0
			  ,gerProHpMax=0
			  ,gerSp=Sp
			  ,gerQuality=Transmigration
			  ,stoneEffectType=0
			  ,spMax=0
			  ,maxAwakeStep=0
              ,gerBody=0}].

add_born_list(GerTypeID,List)                                                                                                                 ->
    case lists:keytake(GerTypeID,1,List) of
        false ->
			[{GerTypeID,1}|List];
        {value,{_,N},RestList} ->
            [{GerTypeID,N+1}|RestList]
    end.

%% 战斗单位结构
ger2p_fighter(Ger)                                                                                                                            ->
	#ger{gerID=GerID,gerBase=GerBase,gerAttr=GerAttr,gerHp=GerHp,gerSp=GerSp,gerProHp=ProHp}=Ger,
    #gerBase{gerPos=GerPos,gerTypeID=GerTypeID,gerQuality=GerQuality,gerAwakeInfo=GerAwakeInfo,gerBody=GerBody} = GerBase,
	#p_fighter{
			   gerID=GerID,
			   gerTypeID=GerTypeID,
			   gerPos=GerPos,
			   gerHp=GerHp,
               gerProHp=ProHp,
			   gerSp=GerSp,
			   gerHpMax=GerAttr#gerAttr.gerHpMax,
               gerProHpMax=GerAttr#gerAttr.gerProHpMax,
			   gerQuality=trunc(GerQuality),
               stoneEffectType = 0,
			   spMax = GerAttr#gerAttr.gerSpMax,
               maxAwakeStep=role_awake:get_max_awake_step(GerAwakeInfo),
               gerBody=GerBody}.			   

%% 判断是否已经分出胜负
check_win(IsChange) ->
    check_win(?get_alives(), IsChange).

%% 两边一起死,算攻击方输
check_win([],_IsChange)      ->
    {ok,false};
check_win(Alives,IsChange)                      ->
   check_win(Alives,IsChange,false,false). 

check_win([],_IsChange,AtkState,_)        ->
    {ok,AtkState};

check_win([#fighter_info{pos=Pos}|T],IsChange,AtkState,DefState)  when abs(Pos) < 10  ->
    {AtkState2,DefState2} = 
        case Pos > 0 of 
            true ->
                {true,DefState};
            _ ->
                {AtkState,true}
        end,
    case AtkState2 andalso DefState2 of
        true ->
            no;
        _ ->
            check_win(T,IsChange,AtkState2,DefState2)
    end;
check_win([_|T],IsChange, AtkState,DefState) ->
	check_win(T,IsChange,AtkState,DefState).

do_attack(#data_skill{defendActionID=DefendActionID}=DataSkill
		 ,SrcGerPos,TarPosList,UniqueHook,#attack_info{use_append=UseAppend}=Acc)                                                ->
	NewAcc = #attack_info{action=AtkAcc,dmgPlus=DmgPlusAcc,uaction=Uaction,unhandleds=UnhandledsAcc,holy_kill_list=HKList,
                            absorb=AbsorbT,dmgBack=DmgBackT0,type=Type,damage=Damage,no_injury_list=_NIList,hurt_cure_list=HCList} =
        lists:foldl(fun(TarGerPos,AtkAcc) ->
                            attack(DataSkill,SrcGerPos,TarGerPos,TarPosList,UniqueHook,AtkAcc) 
				end,Acc,TarPosList),
	{HollyKillL2,AtkAcc2} = case HKList of [] -> {[],AtkAcc};
					_ -> {HKL1,HKL2} = lists:foldl(fun({1,_},{A1,A2})->{[1|A1],A2};
													  ({2,P},{A1,A2})->{A1,[P|A2]}
												   end, {[],[]}, HKList),
						 AtkAccInHK1 = case HKL1 of [] -> AtkAcc;_->[?action(200,SrcGerPos,[],0,0,?STATE_DEFAULT)|AtkAcc] end,
						 {HKL2,AtkAccInHK1}
					   end,

	%AtkAcc2 = case NIList of [] -> AtkAcc2_0;
	%			  _ -> [?action(258,NIGerPos,[],0,0,?STATE_DEFAULT)||NIGerPos<-NIList]++
	%					   [?action(256,NIGerPos,[],0,0,?STATE_DEFAULT)||NIGerPos<-NIList]++AtkAcc2_0
	%		  end,
	%NIAction = [?action(257,NIGerPos,[],0,0,?STATE_DEFAULT)||NIGerPos<-NIList]++[?action(255,NIGerPos,[],0,0,?STATE_DEFAULT)||NIGerPos<-NIList],
	
    %% 处理绝技附加效果
    %% 此处存在当玩家在attack过程中被链接伤害打死的情况下，此处仍然会给对应的精灵挂上链子
    UnhandledActions = deal_c1_unhandledIs(SrcGerPos,DataSkill,UnhandledsAcc),
	#attack_info{uaction=AfterUnHandledActions} = after_unheadledIs_hook(UniqueHook,SrcGerPos,TarPosList,NewAcc#attack_info{uaction=[]}),
    %% 伤害加深的action在攻击结束后播放
	#unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(SrcGerPos), 
	#unique_effect{cure_sub=CurSub,cure_add=CurAdd,invinciblep=Invinciblep} = UniqueBuffer,
	#ger{gerHp=GerHp,gerProHp=GerProHp,gerBase=#gerBase{gerTypeID=SrcGerTypeID,gerPos=SrcGerPos}} = SrcGer = ?get_ger(SrcGerPos),

    %% 处理normal_dot的dot效果
    NorDotActions = 
        case Type of
            normal_dot ->
                #append_attack{args={Percent, Duration}} = UseAppend,
                #attack_info{uaction=NDAction} = add_dot_buffer(TarPosList, #attack_info{damage=Damage},Duration,[1,Percent],SrcGerPos),
                NDAction;
            _ ->
                []
        end,
	
	CurAdd2 = calc_tr_ctl_add(CurAdd,SrcGerTypeID,SrcGerPos),
	%% 再计算主动吸血action
    {AbsorbV, AbsorbActionList} = 
	    case AbsorbT =< 0 of
            true ->
		        {AbsorbT, []};
            _ ->
                %% 治疗减益+增益
                AbsorbT2 = erlang:trunc(AbsorbT * (100 - CurSub + CurAdd2/100) / 100),
		        {AbsorbT2, [?action(?ACTION_ABSORB,SrcGerPos,[],AbsorbT2,0,?STATE_DEFAULT)]}
		end,
	
	%% 再运算反弹
	DamageBackActionList = 
		if Invinciblep ->
			   SrcAddHp = AbsorbV,
			   SrcGerProHp2 = GerProHp,
			   InvAction = [],
			   [];
		   true ->
			   {Inv0,DmgBackT} = check_d_minHp(SrcGerPos,abs(DmgBackT0)),
			   InvAction = if Inv0 -> set_d_inv(); true -> [] end,
			   case DmgBackT =< 0 of
				   true ->
					   SrcAddHp = AbsorbV,
					   SrcGerProHp2 = GerProHp,
					   [];
				   _ ->
					   {ProHpDecf,HpDecf} = decf_ger_hp(GerProHp,DmgBackT),
					   SrcAddHp = AbsorbV + HpDecf,
					   SrcGerProHp2 = GerProHp + ProHpDecf,
					   %% 计算伤害链接
					   #attack_info{action=DBActionList} = 
									   do_damage_link(erlang:hd(TarPosList),SrcGerPos,
													  #attack_info{action=[?pro_action(?ACTION_DAMAGE_BACK,SrcGerPos,[],HpDecf,ProHpDecf,0,?STATE_DEFAULT)]
																   ,damage=DmgBackT}),
					   DBActionList 
			   end
		end,

	%% 计算SrcGer增加的怒气
	SrcAddSp = cacl_add_sp(DataSkill, UseAppend),
	%% 计算新的SrcGer
	SrcHp1 = add_hp(GerHp,?a(SrcGer,gerHpMax),SrcAddHp),
	%% 重新扣除一下反弹的血量
	% SrcHp2 = add_hp(SrcHp1,?a(SrcGer,gerHpMax),DmgBackT),
	SrcHp2 = add_hp(SrcHp1,?a(SrcGer,gerHpMax),0),
    SrcAddSp2 = real_add_sp(SrcGer#ger.gerSp,SrcAddSp,(SrcGer#ger.gerAttr)#gerAttr.gerSpMax),
    SrcSp2 = SrcGer#ger.gerSp + SrcAddSp2,
	SrcGer2 = SrcGer#ger{gerHp=SrcHp2,gerSp=SrcSp2,gerProHp=SrcGerProHp2},
	
	?set_ger(SrcGer2),
	#attack_info{uaction=AfterUniqueAttackAction} = after_unique_attack(UniqueHook, DataSkill#data_skill.skillType,SrcGerPos,TarPosList,NewAcc#attack_info{uaction=[]}),
	%% 攻击者死亡action
	if SrcHp2 =< 0 ->
            %%这里被反弹死的,所以击杀者的pos传0.被反弹死了,需要从队列中剔除掉!
           {IsReborn,RebornAction2,SrcKillHooks} = do_reborn(0,SrcGerPos,{0,[],[]}),
		   if IsReborn == false ->
						  StandFellowAction = stand_fellow(SrcGerPos),
				  Uaction2 = Uaction,
				  C2Action = deal_c2_unhandledIs(SrcGerPos,DataSkill,UnhandledsAcc),
				  SrcDeadAction = StandFellowAction ++ SrcKillHooks ++ [?action(?ACTION_DEAD,SrcGerPos,[],0,0,?STATE_DEFAULT)];
			  true ->
				  Uaction2 = [],
				  C2Action = [],
				  SrcDeadAction = RebornAction2
		   end;
	   %%攻击者复活action
	   true ->
		   Uaction2 = Uaction,
		   C2Action = deal_c2_unhandledIs(SrcGerPos,DataSkill,UnhandledsAcc),
		   SrcDeadAction = []
	end,
	
	%% 计算攻击者的Action
	SrcAttackAction = ?pro_action(DataSkill#data_skill.attackActionID,SrcGerPos,TarPosList,0,0,SrcAddSp2,?STATE_DEFAULT),
	%% 计算复活只需要处理本次攻击的精灵以及他们链接的精灵,击杀的action进行合并,发给客户端
	{_,ActionListReborn,KillHooks} = 
		lists:foldl(fun(GerPos,DAcc)-> 
							GerLocal = ?get_ger(GerPos),
							case GerLocal#ger.gerHp =< 0 of
								true ->
									{A,B,C} = do_reborn(SrcGerPos,GerPos,DAcc) ,
									StandFellowActionLocal = stand_fellow(GerPos),
									{A,StandFellowActionLocal++B,C};
								_ ->
									DAcc
							end					
					end,{0,[],[]},get_all_link(TarPosList)),
	HollyKillDiamondAction = 
		case HollyKillL2 of []->[];
			_->HKMaxHarm = 200000000000,
			   {HollyBAcc,HollyAAcc} = 
				   lists:foldl(fun({HKTarGerPos,_HKTarGerID},{HKBAcc,HKAAcc})->
									   #ger{gerID=_HKNTarGerID,gerHp=HKGerHp,gerProHp=HKProHp,gerAttr=#gerAttr{gerHpMax=HKGerHpMax}} = HKGer = ?get_ger(HKTarGerPos),
									   if HKGerHp > 0 ->
											  HKHarmValueT = HKGerHp+HKProHp,
											  if HKHarmValueT >= HKMaxHarm -> 
													 {HKInv0,HKMaxHarmT1} = check_d_minHp(HKTarGerPos,HKMaxHarm),
													 HKInvAction = if HKInv0 -> set_d_inv(); true -> [] end,
													 HKGerHp2 = add_hp(HKGerHp,HKGerHpMax,-HKMaxHarmT1),
													 HKGer2 = HKGer#ger{gerHp=HKGerHp2},
													 damage_stastic(SrcGerPos,HKMaxHarmT1),
													 ?set_ger(HKGer2),
													 {[?action(DefendActionID,HKTarGerPos,[],-HKMaxHarmT1,0,?STATE_DEFAULT)|HKBAcc]
													  ,[?action(227,HKTarGerPos,[],0,0,?STATE_DEFAULT)|HKInvAction]++HKAAcc};
												 true ->
													 {HKInv0,HKMaxHarmT1} = check_d_minHp(HKTarGerPos,HKHarmValueT),
													 HKInvAction = if HKInv0 -> set_d_inv(); true -> [] end,
													 HKGerHp2 = add_hp(HKGerHp,HKGerHpMax,-HKMaxHarmT1),
													 HKGer2 = HKGer#ger{gerHp=HKGerHp2},
													 ?set_ger(HKGer2),
													 damage_stastic(SrcGerPos,HKMaxHarmT1),
													 State =  if HKGerHp2 > 0 -> ?STATE_DEFAULT;true -> ?STATE_DEAD end,
													 {[?action(DefendActionID,HKTarGerPos,[],-HKMaxHarmT1,0,State)|HKBAcc]
													  ,[?action(227,HKTarGerPos,[],0,0,?STATE_DEFAULT)|HKInvAction]++HKAAcc}
											  end;
										  true ->
											  {HKBAcc,HKAAcc}
									   end
							   end,{[],[?action(226,SrcGerPos,[],0,0,?STATE_DEFAULT)]},HollyKillL2),
			   if HollyBAcc == [] -> [];true -> HollyBAcc++HollyAAcc end
		end,
	
	EnemyAlive = enemy_alive(SrcGerPos),
	EnemyAliveN = erlang:length(EnemyAlive),
	HurtCureActionList = 
		if EnemyAliveN > 0 ->
			   lists:foldl(fun(HCInfo,HCActionAcc)-> 
								   case HCInfo of
									   {{1,HCRList},HCHarmValue,HCPos} ->
										   HCR = util:random_one_from_weigh_list(HCRList),
										   #ger{gerHp=LHCGerHp,gerAttr=#gerAttr{gerHpMax=LHCGerHpMax}} = LGer = ?get_ger(HCPos),
										   if LHCGerHp > 0 andalso HCR > 0 ->
												  HCAddHp = erlang:trunc(HCR * HCHarmValue / 100),
												  LHCGerHp2 = add_hp(LHCGerHp,LHCGerHpMax,HCAddHp),
												  LGer2 = LGer#ger{gerHp=LHCGerHp2},
												  LHCGerHpAdd = LHCGerHp2 - LHCGerHp,
												  ?set_ger(LGer2),
												  [?action(260,HCPos,[],LHCGerHpAdd,0,?STATE_DEFAULT),?action(259,HCPos,[],0,0,?STATE_DEFAULT)]++HCActionAcc;
											  true ->
												  HCActionAcc
										   end;
									   {{2,HCRList},HCHarmValue,HCPos} ->
										   HCR = util:random_one_from_weigh_list(HCRList),
										   #ger{gerHp=HLHCGerHp} = ?get_ger(HCPos),
										   if HLHCGerHp > 0 andalso HCR > 0  ->
												  HCAddHp = erlang:trunc(HCR * HCHarmValue / 100),
												  lists:foldl(fun(CHCPos,CHCActionAcc) ->
																	  #ger{gerHp=LHCGerHp,gerAttr=#gerAttr{gerHpMax=LHCGerHpMax}} = LGer = ?get_ger(CHCPos),
																	  LHCGerHp2 = add_hp(LHCGerHp,LHCGerHpMax,HCAddHp),
																	  LGer2 = LGer#ger{gerHp=LHCGerHp2},
																	  LHCGerHpAdd = LHCGerHp2 - LHCGerHp,
																	  ?set_ger(LGer2),
																	  [?action(260,CHCPos,[],LHCGerHpAdd,0,?STATE_DEFAULT)|CHCActionAcc]
															  end, [?action(261,HCPos,[],0,0,?STATE_DEFAULT)|HCActionAcc], EnemyAlive);
											  true ->
												  HCActionAcc
										   end
								   end
						   end,[],HCList);
		   true ->
			   []
		end,
	#attack_info{uaction=Uaction1} = NewAcc1 = case Type of	
												 normal_dot -> after_normal_dot_hook(UniqueHook,SrcGerPos,TarPosList,NewAcc);
								%				 unique_noSp_2 -> hit_unique_hook(UniqueHook,SrcGerPos,TarPosList,NewAcc_0);
												 _ -> unique_hit_hook(UniqueHook,SrcGerPos,TarPosList,NewAcc#attack_info{uaction=[]}) 
											 end,
	{_,ActionListReborn1,KillHooks1} = 
        lists:foldl(fun(GerPos,DAcc)-> 
                        							GerLocal = ?get_ger(GerPos),
							case GerLocal#ger.gerHp =< 0 of
								true ->
									{A,B,C} = do_reborn(SrcGerPos,GerPos,DAcc) ,
									StandFellowActionLocal = stand_fellow(GerPos),
									{A,StandFellowActionLocal++B,C};
								_ ->
									DAcc
							end
                    end,{0,[],[]},enemy_alive(SrcGerPos)),
	NewAction = NorDotActions ++ AfterUnHandledActions++UnhandledActions ++ DmgPlusAcc ++Uaction2++ C2Action++
					KillHooks1 ++ ActionListReborn1 ++ Uaction1++HurtCureActionList++HollyKillDiamondAction++
					AfterUniqueAttackAction++KillHooks ++ ActionListReborn ++SrcDeadAction++ DamageBackActionList ++
					InvAction++AbsorbActionList ++ AtkAcc2 ++ [SrcAttackAction],%++NIAction,
    NewAcc2 = NewAcc1#attack_info{action=NewAction,holy_kill_list=[],uaction=[],no_injury_list=[],hurt_cure_list=[]},
    #ger{gerHp=SrcGerHp} = ?get_ger(SrcGerPos),
    %% 反击判断
    case SrcGerHp =< 0 of
        true ->
            NewAcc2;
        _ ->
            check_strike_back(DataSkill,SrcGerPos,NewAcc2)
    end.

apply_attack(_Skills,_,_DataSkill,_SrcGerPos,_TarPosList,Acc =#attack_info{count=NowCC,type=Type,maxCount=MaxCC})
  when Type /= normal_2 andalso Type /= unique_noSp andalso Type /= unique_noSp_2 andalso Type /= normal_dot 
  andalso Type /= unique_critic andalso Type /= unique_noSp_3 andalso Type /= unique_noSp_4 andalso NowCC == MaxCC ->
    Acc;
apply_attack(Skills,IsChange,DataSkill,SrcGerPos,TarPosList,#attack_info{action=ActionAcc,count=Count,maxCount=MaxCC}=Acc)                        ->
	#ger{gerID=GerID1} = ?get_ger(SrcGerPos),
	UniqueHook = ?get_unique_hook(SrcGerPos),
	%% 放必杀前、追击前的附加技能
	Accbuh = before_unique_hook(UniqueHook,SrcGerPos,Acc),
	#attack_info{uaction=Uaction} = before_attack_hook(UniqueHook, SrcGerPos,Accbuh),
	%% 攻击时传入的action为空,返回的action是这次攻击的所有action
	Acc2 = do_attack(DataSkill,SrcGerPos,TarPosList,UniqueHook,Acc#attack_info{action=[],uaction=[],damage=0,direct=0,atk_skill=DataSkill}),
	Acc3= after_normal_attack_hook(UniqueHook, DataSkill#data_skill.skillType,SrcGerPos,TarPosList, Acc2),
	#attack_info{action=AtkAction,append=Append,type=Type,afteraction=AfterAction} =last_action_hook(UniqueHook, SrcGerPos,TarPosList,Acc3),
	%% 获得追加攻击的信息
	{UseAppend,AAtkList,AAtkSKill,AAction,NewAppend} = get_append_attack_info(SrcGerPos,Skills,DataSkill,Append,Type,Count),
	%% 一定要合并action
	case check_win(IsChange) of
		{ok,Result} ->
			{ok, Result, AfterAction ++ AtkAction ++ Uaction ++ ActionAcc};
		no ->
			#ger{gerID=GerID2} = ?get_ger(SrcGerPos),
			case GerID1 == GerID2 of
				true ->
					%% normal_dot是带附加的追加攻击,所以需要用use_append来保存数据
					%% type和atk_skill需要传输到外部去,给战斗动作结束后触发的判断使用
					case UseAppend of
						undefined ->
							#attack_info{action=AfterAction ++AtkAction ++ Uaction ++ ActionAcc,atk_skill=DataSkill,type=Type};
						#append_attack{type=AAtkType} ->
							case ?is_maimed(SrcGerPos) of
							true ->
								#attack_info{action=AfterAction ++AtkAction ++ Uaction ++ ActionAcc,atk_skill=DataSkill,type=Type};
							_ ->
								NewAcc2 = #attack_info{action=AAction ++ AfterAction ++AtkAction ++ Uaction ++ ActionAcc,type=AAtkType
											,count=Count+1,use_append=UseAppend,append=NewAppend,atk_skill=DataSkill,maxCount=MaxCC},
								apply_attack(Skills,IsChange,AAtkSKill,SrcGerPos,AAtkList,NewAcc2) 
							end
					end;
				_ ->
					#attack_info{action=AfterAction ++AtkAction ++ Uaction ++ ActionAcc,atk_skill=DataSkill,type=Type}
			end
	end.

%% 计算攻击者的怒气改变
cacl_add_sp(DataSkill, #append_attack{type=Type}) ->
    cacl_add_sp(DataSkill, Type);

cacl_add_sp(#data_skill{skillType=SkillType}, AppendType) ->
    IsUnique = 
        case SkillType of
            unique -> 
                true;
            unique2 ->
                true;
            _ ->
                false 
        end,
    cacl_add_sp_1(IsUnique, AppendType).

%cacl_add_sp_1(false, normal_2) ->			0;
cacl_add_sp_1(false, normal_crazy)->		0;
cacl_add_sp_1(false, _) ->					?BEAT_ADD_SP;
%% 三头龙的必杀追击,不算作必杀,所以不扣能量
cacl_add_sp_1(true, unique_critic) ->		?BEAT_ADD_SP;
cacl_add_sp_1(true, unique_noSp) ->			0;
cacl_add_sp_1(true, unique_noSp_2) ->		0;
cacl_add_sp_1(true, unique_noSp_3) ->		0;
cacl_add_sp_1(true, unique_noSp_4) ->		0;
cacl_add_sp_1(true, _) ->					-100.

%% miss_list 标识本轮被miss的所有精灵
clean_miss_data()->
	set_miss_list([]).
mark_miss(Pos) ->
	set_miss_list([Pos|get_miss_list()]).
set_miss_list(List) ->
	erlang:put(miss_list, List).
get_miss_list()->
	get(miss_list).

attack(DataSkill,SrcGerPos,TarGerPos,TarPosList,UniqueHook,#attack_info{action=AtkAction,type=Type}=AtkAcc)                       ->
    NewAction = attack_hook(SrcGerPos,TarGerPos,AtkAction),
    NewAtkAcc = AtkAcc#attack_info{action=NewAction},
    SrcGer = erlang:get(SrcGerPos),
	TarGer = erlang:get(TarGerPos),
	%% 攻击方的绝技命中加成
	#unique_buffer{use=SrcUniqueBuffer} = ?get_unique_buffer(SrcGerPos), 
	#unique_effect{hit_plus=SrcHitPlus,hit_critic={HitCritc,_},spRa=SpRa} = SrcUniqueBuffer,
    %% 训练师必定命中的技能
    case HitCritc of
        true ->
			NewAtkAcc3 = hit_unique_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,NewAtkAcc),
			attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc3);
        _ ->
            case Type of
                %%追加的必然命中和暴击的普通攻击
                hit_critic ->
                    attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc);
                unique_critic ->
					NewAtkAcc_2 = after_crit_hook(UniqueHook,SrcGerPos,TarPosList,NewAtkAcc),
                    attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc_2);
				unique_critic_2 ->
					attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc);
				unique_critic_3 ->
					attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc);
				normal_crazy ->
					NewAtkAcc3 =  hit_unique_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,NewAtkAcc),
					attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc3);
                _ ->
        	        Doom = ?a(TarGer,gerMiss)-?a(SrcGer,gerDoom)-DataSkill#data_skill.gerDoom - SrcHitPlus,
                    case Doom =< 0 of
                        true ->
                            NewAtkAcc3 = hit_unique_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,NewAtkAcc),
                            attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc3);
        	            _ ->
        		            RandomValue = random:uniform(),
        		            case RandomValue =< 1/(1+Doom/100) of
        			            true ->
                                    NewAtkAcc3 = hit_unique_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,NewAtkAcc),
                                    attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,NewAtkAcc3);
        			            false ->
        							mark_miss(TarGerPos),
                                    %% 无敌时闪避不加怒气
                                    #unique_buffer{use=TarUniqueBuffer} = ?get_unique_buffer(TarGerPos),
                                    #unique_effect{invinciblep=TarInvinciblep} = TarUniqueBuffer,
                                    {TarAddSp,TarGer2} = 
                                        case TarInvinciblep of
                                            true ->
                                                {0,TarGer};
                                            _ ->
                                                TarAddSpT = trunc(real_add_sp(TarGer#ger.gerSp,?HURT_ADD_SP,(TarGer#ger.gerAttr)#gerAttr.gerSpMax) * SpRa / 100),
        				                        TarSp2 = TarGer#ger.gerSp + TarAddSpT,
        				                        {TarAddSpT,TarGer#ger{gerSp=TarSp2}}
                                        end,
                                    erlang:put(TarGerPos,TarGer2),
                                    #attack_info{action=NewAtkAction,miss=MissAcc} = NewAtkAcc,	
                                    MissAction = ?miss_action(TarGer2,TarAddSp),
                                    NewAtkAcc3 = miss_unique_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,NewAtkAcc#attack_info{action=[MissAction|NewAtkAction]}),
                                    NewAtkAcc3#attack_info{miss=[miss|MissAcc]}
        		            end
                    end
        	end 
        end.

%% 攻击伤害的具体运算
%% FIXME calc_skill_damage 的代码和这儿计算伤害的代码相互关联，如果这儿修改伤害计算，那儿也需要修改
attack2(UniqueHook,DataSkill,SrcGer,TarGer,TarPosList,#attack_info{multirecord=MR,type=Type,count=Count,direct=Direct
																   ,holy_kill_list=HKList,no_injury_list=NIList,hurt_cure_list=HCList} = AtkAcc) ->
	#data_skill{defendActionID=DefendActionID,damageRatio=DamageRatio1,gerReel=AddReel,skillType = _SkillType,
				damageType=DamageType,gerCritic=AddCritic,gerAbsorb=AddAbsorb}=DataSkill,
	#ger{gerBase=#gerBase{gerPos=SrcGerPos,gerTypeID=SrcGerTypeID},gerAttr=SrcAttr2}=SrcGer,
	#ger{gerID=TarGerUID,gerBase=#gerBase{gerPos=TarGerPos,gerTypeID=TarGerTypeID},gerAttr=#gerAttr{gerHpMax=TarGerHpMax}=TarAttr2,gerHp=TarHp,gerSp=TarSp,gerProHp=TarPropHp}=TarGer,
	#dynamic_buffer{attr=SrcDynamicBuffer} = erlang:get({dynamic_buffer,SrcGerPos}),
	#dynamic_buffer{attr=TarDynamicBuffer} = erlang:get({dynamic_buffer,TarGerPos}),
	#unique_buffer{use=SrcUniqueBuffer} = ?get_unique_buffer(SrcGerPos),
	#unique_buffer{use=TarUniqueBuffer} = TarUniqueBufferR = ?get_unique_buffer(TarGerPos),
	#unique_effect{critic_plus=SrcCriticPlus,sleep=Sleep,weak=Weak,attack_add=SrcAttackAdd,holy_kill=HolyKill,con_attack=ConAttack,maxCount=MaxCount
				   ,damage_dec=SrcDamageDec,crit_hit_add=CritHitAdd,hit_critic={HitCritc,_},atk_dmg_sub={ADS,_,_},damage_double=DamageDouble
				   ,atk_dmg_plus={ADP,_,_,_},morph_status=SrcMorphStatus,damage_base_blood=DamageBaseBlood,damage_maxHp=DamageMaxHp,superCrit={SCritR,SCritV}
				  } = SrcUniqueBuffer,
	#unique_effect{damage_plus={TarDamagePlus,_,_},damage_sub={TarDamageSub,_},invinciblep=TarInvinciblep,parry=Parry,spRa=SpRa,pm_dmg_dec=PmDmgDec%,inv_way={_InvWay,_}
				   ,damage_add={TarDamageAdd,_},damage_dec=TarDamageDec,morph_status=TarMorphStatus,no_injury=_NoInjury,hurt_cure=HurtCure
				  } = TarUniqueBuffer,
	%%由于魔典、科技、附魔、觉醒的属性加成都已经加入到gerExtra中，所以此处直接使用gerExtra即可
	SrcEnchantAdd = SrcGer#ger.gerExtra,
	TarEnchantAdd = TarGer#ger.gerExtra,
	%根据技能类型为技能伤害加成添加上附魔加成
	DamageRatio2 = case Type of
					   normal->
						   DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					   normal_2->
						   DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					   normal_3->
						   DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					   normal_4->
						   DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
                       normal_5 ->
                           DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					   unique->
						   DamageRatio1 + SrcEnchantAdd#add_attr.gerUskillDemageRate;
					   unique2->
						   DamageRatio1 + SrcEnchantAdd#add_attr.gerUskillDemageRate;
					   _ ->
						   DamageRatio1
				   end,
	SrcGerSkinBuff = ?get_pos_skinbuff(SrcGerPos),
	TarGerSkinBuff = ?get_pos_skinbuff(TarGerPos),
	DamageRatio = erlang:max((DamageRatio2+SrcGerSkinBuff#skin_buff.ger_attack_plus-TarGerSkinBuff#skin_buff.ger_demage_sub),1),
	%% 在这儿加上动态属性
	SrcAttr = add_talent_to_gerAttr(SrcAttr2,SrcDynamicBuffer),
	TarAttr = add_talent_to_gerAttr(TarAttr2,TarDynamicBuffer),
	%% 计算击晕
	%	Reel = SrcAttr#gerAttr.gerReel + AddReel - TarAttr#gerAttr.gerReelReduce,
	ReelBase = 2.5 + (SrcAttr#gerAttr.gerReel + AddReel - TarAttr#gerAttr.gerReelReduce) / 100 ,
	ReelBase1 = if ReelBase == 0 -> 2; 
				   ReelBase < 0 -> abs(ReelBase);
				   true -> ReelBase
				end,   %% maybe, ReelBase is 0.0
	Reel =  1 - 2.5 / ReelBase1,
	ReelState =
		case Reel =< 0 orelse TarInvinciblep orelse is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
			true ->
				?STATE_DEFAULT;
			_ ->
				RandomValue = random:uniform(),
				case RandomValue =< Reel of %1-1/(1+Reel*0.01) of
					true ->
						%%此处加上对需要解除目标眩晕的target_maimed
						case update_target_maimed(SrcGerPos,TarGerPos,6,1) of
							invinciblep ->
								?STATE_DEFAULT;
							_ ->
								?STATE_REEL 
						end;
					_ ->
						?STATE_DEFAULT
				end
		end,
	
	
	%% 暴击概率
	CritProb0 = 
		%% 第四次必然不暴击
		case Count =:= MaxCount of
			true ->
				0;
			_ ->
				%% 必定暴击
				if 
					HitCritc ->	101;
					Type =:= hit_critic ->101;
					Type =:= unique_critic ->101;
					Type =:= unique_critic_2 ->101;
					Type =:= unique_critic_3 ->101;
					true ->
						Diff = (1 + (SrcCriticPlus / 100)) * (SrcAttr#gerAttr.gerCritic+AddCritic) - TarAttr#gerAttr.gerCriticReduce,
						Diff2 = 0.8 + Diff * 0.01,
						case Diff2 =< 0.000001 of
							true ->
								0;
							_ ->
								1 - 0.8 / Diff2
						end 
				end 
        end,
    CritProb = CritProb0 + SCritR/10000,
    {CritValueT0,CritState,CritHitAddA} = 
        if CritProb =< 0 ->{1,?STATE_DEFAULT,CritHitAdd};
          % CritProb > 100 ->{2,?STATE_CRIT,CritHitAdd};
           true ->		   
               RandomValue2 = random:uniform(),
               case RandomValue2 =< CritProb of
                   true -> RVv = random:uniform(),
                           if RVv < SCritR / CritProb ->{SCritV/10000,?STATE_TRAINER_CRIT,CritHitAdd}; 
                              true -> {2,?STATE_CRIT,CritHitAdd}
                           end;
                   false ->{1,?STATE_DEFAULT,CritHitAdd}
               end
        end,
    
	CritValueT = CritValueT0 * (1 + CritHitAddA / 10000),
	%% 沉睡暴击
	{CritValueT1,WakeupAction} = 
		case Sleep of
			0 ->
				{CritValueT,[]};
			_ ->
				case is_sleep(TarGerPos) of
					false ->
						{CritValueT,[]};
					_ ->
						wakeup(TarGerPos),
						{CritValueT * Sleep,[?action(?ACTION_UNSLEEP,TarGerPos,[],0,0,?STATE_DEFAULT)]}
				end
		end,
	
	%% 虚弱暴击
	CritValue1 = 
		case Type =:= unique andalso Weak =/= [] of
			false ->
				CritValueT1;
			_ ->
				[X,N] = Weak,
				case TarHp / TarGerHpMax * 100 =< X of
					true ->
						CritValueT1 * N;
					_ ->
						CritValueT1
				end
		end,
	
	%% 舞步状态暴击
	CritValue =
		case SrcMorphStatus =:= ?ger_morph_status_dance of
			true ->
				CritValue1 * 2;
			_ ->
				CritValue1
		end,
	IsParry = case Parry of true -> Type == unique orelse Type == normal; _ -> false end,
	%% 无敌的精灵不会受到伤害,也不会反弹伤害(如果也不触发暴击,这把这个判断前移)
	case TarInvinciblep of
		false ->
			%DiaInvAction = [],
			{HolyKillTy,HollyKillRa} = HolyKill,
			IsHolyKill = random:uniform(100) =< HollyKillRa,
			HKList0 = case  IsHolyKill andalso HolyKillTy == 2 andalso (Type == unique orelse Type == normal )of
						  true -> [{HolyKillTy,{TarGerPos,TarGerUID}}|HKList];
						  _ -> HKList
					  end,
			case IsHolyKill andalso HolyKillTy == 1 andalso (Type == unique orelse Type == unique2 orelse Type == unique_critic) of
				true ->
					NIList2 = NIList,
					%IsNoInjury=false,
					HarmValueT1_0 = TarHp+TarPropHp,
					MaxHarm = 200000000000,
					HarmValueT0_1 = if HarmValueT1_0 > MaxHarm -> MaxHarm; true -> HarmValueT1_0 end,
					TarSp2=TarSp,
					HKList2 = [{HolyKillTy,TarGerPos}|HKList0],
					TarAddSp = 0;
					%HKAction = [?action(227,TarGerPos,[],0,0,?STATE_DEFAULT)];
				_ ->
					%HKAction=[],
					HKList2 = HKList0,
					%IsNoInjury = random:uniform(100) =< NoInjury  andalso (Type == normal orelse Type == unique),
					case IsParry of
						false ->
							NIList2 = NIList,
							
							%% 伤害数值随机区间
							{A,B} = data_common:get(DamageType),
							RandomValue3 = random_float(A,B),
							%% 抗性系数
							Defence2 = 
								case erlang:atom_to_list(DamageType) of
									[$p|_] ->
										?physic_defence;
									_ ->
										?magic_defence 
								end,
							
							SrcAttackAdd2 = calc_tr_ctl_add(SrcAttackAdd,SrcGerTypeID,SrcGerPos),
							TarDamageDec2 = calc_tr_ctl_add(TarDamageDec,TarGerTypeID,TarGerPos),
							PmDmgDec2 = if  TarPropHp > 0 -> PmDmgDec/10000; true -> 0 end,
							%% 伤害计算公式
							HarmValueT0_0 = 
								case Direct of
									0 ->
										HarmValueT = RandomValue3 * CritValue * SrcAttr#gerAttr.gerAttack * Defence2 * DamageRatio * 0.0001,
										erlang:max(1,erlang:trunc(HarmValueT * (1 + (ADP + TarDamagePlus - TarDamageSub  - ADS) / 100 
																					+ (SrcEnchantAdd#add_attr.gerExtraDemageRate - TarEnchantAdd#add_attr.gerDecDemageRate) / 10000
																					+ (SrcAttackAdd2 + TarDamageAdd-TarDamageDec2) / 10000
																			   ) * (1 - PmDmgDec2 )* trunc(math:pow(2,DamageDouble))
																 )
												  );
									_ ->
										Direct
								end,
							HarmValueT0 = 
								case Type of unique -> HarmValueT0_0 + trunc(DamageMaxHp * TarGerHpMax / 100);
									_  -> HarmValueT0_0
								end,
							%% 加怒气
							TarAddSp = trunc(real_add_sp(TarSp,?HURT_ADD_SP,(TarGer#ger.gerAttr)#gerAttr.gerSpMax) * SpRa / 100),
							TarSp2 = TarSp + TarAddSp,
							DamageBaseBloodHarm = trunc(TarGerHpMax * DamageBaseBlood / 100),
							HarmValueT0_1 = HarmValueT0 + DamageBaseBloodHarm;
						_ ->
							NIList2 = [TarGerPos|NIList],
							HarmValueT0_1 = 0,
							TarSp2 = TarSp,
							TarAddSp=0
					end
			end,
			{Inv0,HarmValueT1} = check_d_minHp(TarGerPos,HarmValueT0_1),
			InvAction = if Inv0 -> set_d_inv(); true -> [] end,
			
			%% 扣血量(重算血量。。。设置血量不超过最大血量值)
			{ProHpDecf,HpDecf} = decf_ger_hp(TarPropHp,HarmValueT1),
			TarHp_1 = add_hp(TarHp,TarAttr#gerAttr.gerHpMax,HpDecf),
			TarProHp_1 = TarPropHp + ProHpDecf,
			%% 实际伤害数值
			{TotalHarm,ConHarmAction,InvAction,TarProHp2,TarHp2} = 
				do_con_attack(HarmValueT1,AtkAcc,InvAction,TarProHp_1,TarHp_1,TarAttr,TarGerPos,DefendActionID,ConAttack),
			HarmValue = HarmValueT1 + TotalHarm,
			Value = TarHp-TarHp2,
			damage_stastic(SrcGerPos,HarmValue),
			DeadState = 
				case TarHp2 =< 0 of
					true ->
						?STATE_DEAD;
					_ ->
						?STATE_DEFAULT
				end,
			
			%% 吸血 
			Absorb = SrcAttr#gerAttr.gerAbsorb+AddAbsorb,
			if Absorb =< 0 ->
				   AbsortState = ?STATE_DEFAULT,
				   AbsortHp = 0;
			   true ->
				   AbsortState = ?STATE_ABSORB,
				   AbsortHp = erlang:max(1,trunc(HarmValue * Absorb /10000))
			end,
			
			%% 反弹
			Value2 = Value - ProHpDecf,
			if TarHp2 =< 0 ->
				   %% 如果死亡，则不反弹
				   DamageBackState = ?STATE_DEFAULT,
				   DamageBackHp = 0;
			   true ->		   
				   DamageBack = TarAttr#gerAttr.gerDamageBack,
				   if DamageBack =< 0 ->%orelse IsNoInjury ->
						  DamageBackState = ?STATE_DEFAULT,
						  DamageBackHp = 0;
					  true ->
						  DamageBackState = ?STATE_DAMAGE_BACK,
						  DamageBackHp = -erlang:max(1,trunc((Value2 * DamageBack /10000)
																 * (1-SrcEnchantAdd#add_attr.gerDecDemageRate/10000 - SrcDamageDec / 10000)
															)
													)
				   end 
			end,
			%% 攻击附带的状态（暴击、击晕）
			State = ReelState bor AbsortState bor DamageBackState bor DeadState bor CritState,
			% State =  AbsortState bor DamageBackState bor DeadState bor CritState,
			%% 更新血量与怒气
			TarGer2 = TarGer#ger{gerSp=TarSp2,gerHp=TarHp2,gerProHp=TarProHp2},
			erlang:put(TarGerPos,TarGer2);
		_ ->
%% 			DiaInvAction = case lists:member(diamond,InvWay) of
%% 							   true ->	[?action(229,TarGerPos,[],0,0,?STATE_TALENT)];
%% 							   _-> []
%% 						   end,
			HKList2 = HKList,
			NIList2 = NIList,
			State = CritState,
			%HKAction = [],
			InvAction = [],
			ConHarmAction = [],
			HarmValue = 0,
			TarAddSp = 0,
			Value = 0,
			AbsortHp = 0,
			HpDecf=0,
			TarHp2=0,
			ProHpDecf=0,
			DamageBackHp = 0
	end,

	if HurtCure == [] orelse HarmValue == 0 -> HCList2 = HCList;
 	   true -> HCList2 = [{HurtCureM,HarmValue,TarGerPos}||HurtCureM<-HurtCure]++HCList
	end,
	if IsParry ->
		   TarUniqueBuffer2 = TarUniqueBuffer#unique_effect{parry=false},
		   TarUniqueBufferR2  = TarUniqueBufferR#unique_buffer{use=TarUniqueBuffer2},
		   ?set_unique_buffer(TarGerPos, TarUniqueBufferR2),
		   ParryAction = [?action(258,TarGerPos,[],0,0,?STATE_TALENT)];
	   true -> TarUniqueBufferR2 = TarUniqueBufferR,TarUniqueBuffer2=TarUniqueBuffer,ParryAction = []
	end,

    #attack_info{action=AtkAction,dmgPlus=DmgPlusAcc,miss=MissAcc,absorb=AbsorbAcc,dmgBack=DmgBackAcc} = AtkAcc,	
    AtkAcc2 = AtkAcc#attack_info{damage=HarmValue,holy_kill_list = HKList2,no_injury_list=NIList2,hurt_cure_list=HCList2}, %% 防止群攻时时无敌的精灵把伤害置0了
	%% 受击action
 	NewAtkAction = ParryAction++InvAction++ConHarmAction++[?pro_action(DefendActionID,TarGerPos,[],HpDecf,ProHpDecf,TarAddSp,State)]++AtkAction,
    %% 伤害加深action在这儿产生,但是在所有攻击放完后一起放
    NewDmgPlusAcc = 
        %% 死亡时不放,且只在触发那次放
        case TarDamagePlus > 0 andalso TarHp2 > 0 of
            true ->
                %% 这个列表数量较小,做一次遍历是可以接受的,数据量大时可以将ApplyType单独存入一个list中
                case lists:any(fun(E) -> 
                                #data_unique_effect{apply_type=ApplyType} = data_unique_effect:get(E),
                                ApplyType =:= 3 end,MR) of
                    true ->
                        [?action(107,TarGerPos,[],0,0,?STATE_TALENT)|DmgPlusAcc];
                    _ ->
                        DmgPlusAcc
                end;
            _ ->
                DmgPlusAcc
        end,
    %% 处理伤害链接
    AtkAcc3 = do_damage_link(SrcGerPos,TarGerPos,AtkAcc2#attack_info{action= NewAtkAction}),
    %% 在连接造成伤害之后处理爆炸
    TarGer5 = #ger{gerHp=TarHp5} = ?get_ger(TarGerPos),
	{ExplosedHpT,ExplosedAction} = 
		case TarHp5 >0 of
			true->
				get_awake_special_action(SrcGerPos,TarGerPos,Value,TarHp5,Type);
			false->
				{0,[]}
		end,
	if ExplosedHpT /= 0 ->
		   {Inv1,ExplosedHp} = check_d_minHp(TarGerPos,ExplosedHpT),
		   InvAction1 = if Inv1 -> set_d_inv(); true -> [] end,
		   damage_stastic(SrcGerPos,ExplosedHp),
		   TarHp6 = add_hp(TarHp5,TarAttr#gerAttr.gerHpMax,ExplosedHp),
		   
		   ?set_ger(TarGer5#ger{gerHp=TarHp6});
	   true ->
		   InvAction1 = [],
		   ExplosedHp = 0
	end,
    %% 在这儿处理绝技暴击的trigger
    #attack_info{uaction=UactionT} = NewAtkAcc =
        case Type =:= unique andalso (CritState =:= ?STATE_CRIT orelse CritState =:= ?STATE_TRAINER_CRIT) of
            false ->
                AtkAcc3;
            _ ->
                unique_crit_hook(UniqueHook,SrcGerPos,TarGerPos,AtkAcc3)        
        end,
	NewAtkAcc2 = NewAtkAcc#attack_info{dmgPlus=NewDmgPlusAcc,miss=[Value|MissAcc],absorb=AbsorbAcc+AbsortHp,dmgBack=DmgBackAcc+DamageBackHp,afteraction=NewAtkAcc#attack_info.afteraction++InvAction1 ++ ExplosedAction++WakeupAction},
    % {SecondLinkAction,AttackInfo} = do_damage_link2(SrcGerPos,TarGerPos,NewAtkAcc2),
    %%这里处理下爆炸伤害造成的伤害
    
    Result = do_damage_link2(SrcGerPos,TarGerPos,NewAtkAcc2#attack_info{damage=ExplosedHp}),
    {SecondLinkAction,AttackInfo} = Result,
    %% 处理造成伤害后的技能
%    #attack_info{action=ActionList,uaction=Uaction} = NewAtkAcc3_0 =  after_damage_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,AttackInfo#attack_info{damage=HarmValue,uaction=[],afteraction=SecondLinkAction++AttackInfo#attack_info.afteraction}),
	NewAtkAcc3_0 =  after_damage_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,AttackInfo#attack_info{damage=HarmValue,uaction=[],afteraction=SecondLinkAction++AttackInfo#attack_info.afteraction}),
	#attack_info{action=ActionList,uaction=Uaction} = NewAtkAcc3 =  any_hurt_hook(UniqueHook,SrcGerPos,TarGerPos,TarPosList,NewAtkAcc3_0),

    %% 反击触发判断(残废的不触发反弹),UactionT是为了保留攻击开始到结束之期间的action
    God2Skill = 
        case lists:keytake(TarGerPos,#fighter_info.pos,?get_alives()) of
            false ->
                [];
		    {value,#fighter_info{skill=#gerSkill{god2=God3Skill}},_QueueAcc2} ->
		    	God3Skill
        end,
    #ger{gerHp=TarGerHp} = ?get_ger(TarGerPos),
    BornResult = (TarGerHp =<0 andalso not ?is_reborn(TarGerPos) andalso God2Skill =/=[]),
    TarUniqueHook = ?get_unique_hook(TarGerPos),
    NewAtkAcc4 = 
        case ?is_maimed(TarGerPos) andalso not BornResult of
       	    true -> 
                  maimed_hurt_hook(TarUniqueHook, TarGerPos, SrcGerPos, NewAtkAcc3#attack_info{action=Uaction ++ ActionList,uaction=UactionT});
            _ ->
                after_unique_hit_hook(TarUniqueHook, TarGerPos, SrcGerPos, NewAtkAcc3#attack_info{action=Uaction ++ ActionList,uaction=[],damage=ExplosedHp+HarmValue}) 
        end,

    #attack_info{action=ActionList2,uaction=Uaction2} = NewAtkAcc4,
    Uaction3 = Uaction2 ++ ActionList2,

    %% 处理沉睡变身
    case TarMorphStatus =:= ?ger_morph_status_sleep of
        true ->
            {MorphActions, {Ratio, AtkSkill}, TarUniqueEffect2} = morph_sleep_wakeup(TarGerPos, TarUniqueBuffer2),
            rm_old_target_maimed_info(TarGerPos, TarGerPos, ger_morph_status_sleep),
            ?set_unique_buffer(TarGerPos, TarUniqueBufferR2#unique_buffer{use=TarUniqueEffect2}),
            NewAtkAcc4#attack_info{action=MorphActions ++ Uaction3,uaction=UactionT,mstrike_back=[{TarGerPos, Ratio, AtkSkill,TarGerTypeID}]};
			%after_strike_back(?get_unique_hook(TarGerPos),TarGerPos,SrcGerPos,NewAtkAcc4_1); 
        _ ->
            NewAtkAcc4#attack_info{action=Uaction3,uaction=UactionT} 
    end.

do_con_attack2(HarmValue,Count, DefendActionID,TarGerPos,HarmAction,TarProHp,TarHp,HpMax,InvAction) when InvAction == [] andalso TarHp > 0 andalso Count > 0 ->
	{Inv0,HarmValue} = check_d_minHp(TarGerPos,HarmValue),
	InvAction2 = if Inv0 -> set_d_inv(); true -> [] end,
	{ProHpDecf,HpDecf} = decf_ger_hp(TarProHp,HarmValue),
	TarHp2 = add_hp(TarHp,HpMax,HpDecf),
	TarProHp2 = TarProHp + ProHpDecf,
	HarmAction2 = [?pro_action(DefendActionID,TarGerPos,[],HpDecf,ProHpDecf,0,?STATE_DEFAULT)|HarmAction],
	do_con_attack2(HarmValue,Count-1,DefendActionID,TarGerPos,HarmAction2,TarProHp2,TarHp2,HpMax,InvAction2);
do_con_attack2(_,Count,_,_,HarmAction,TarProHp,TarHp,_,InvAction) ->
	{HarmAction,Count,TarProHp,TarHp,InvAction}.
do_con_attack(HarmValue,#attack_info{type=unique_noSp_3,count=Count,use_append=#append_attack{args=Arg}},InvAction,TarProHp,TarHp,TarAttr,TarGerPos,DefendActionID,true) ->
	HarmValue2 = HarmValue * hd(Arg) div 100,
	if HarmValue2 == 0 ->
		   {0,[],InvAction,TarProHp,TarHp};
	   true ->
		   {HarmAction,Count2,TarProHp2,TarHp2,InvAction2} 
			   =do_con_attack2(HarmValue2,Count,DefendActionID,TarGerPos,[],TarProHp,TarHp,TarAttr#gerAttr.gerHpMax,InvAction),
		   TotalHarm = HarmValue2 * (Count - Count2),
		   {TotalHarm,HarmAction,InvAction2,TarProHp2,TarHp2}
	end;
do_con_attack(_,_,InvAction,TarProHp,TarHp,_,_,_,_) ->
	{0,[],InvAction,TarProHp,TarHp}.

real_add_sp(TarSp,AddSp,MaxSp)->
	if AddSp >= 0 ->
		   if 
                TarSp >= MaxSp  ->
				    0;
                true ->
                    erlang:min(AddSp,MaxSp - TarSp)
		   end;
	   true ->
		   if TarSp + AddSp =< 0 ->
				  -TarSp;
			  true ->
				  AddSp
		   end
	end.

calc_tr_ctl_add(0,_,_) ->
	0;
calc_tr_ctl_add(TarDamageAdd,SrcGerTypeID,SrcGerPos) ->
	{IDList,Add} = 
	case get({?trSpecial,?sign(SrcGerPos)}) of
		undefined ->
			{[],0};
		L ->
			L
	end,
	#data_ger{gerProperty=SrcGerSpecialID} = data_ger:get(SrcGerTypeID),
	case lists:member(SrcGerSpecialID,IDList) of
		true ->
	TarDamageAdd +Add;
		_ ->
			TarDamageAdd
  end.

%% 加血量
add_hp(Hp,HpMax,AddHp) ->

	Result = erlang:max(0,erlang:min(HpMax,Hp+AddHp)),
	%?DEBUG("HP:~w HpMax:~w AddHp:~w Hp2:~w ~n",[Hp,HpMax,AddHp,Result]),
	Result.

%% 检查是否有活着的
select_alive(PosList) ->
    Alives = ?get_alives(),
	lists:foldl(fun(Pos,Acc) -> 
                    case lists:keyfind(Pos,#fighter_info.pos,Alives) of
                        false ->
                            Acc;
                        _ ->
                            [Pos|Acc]
                    end
			    end,[],PosList).

%% 选者活着的,没有着筛选下一个
select_unique_append_alive([],_,_) ->
    [];

%% 先在后续的里面找,如果没有则从头开始找
select_unique_append_alive(PosList,Skill,SrcPos) ->
    case select_alive(PosList) of
        [] ->
            %% 要求的是如果被追击的都死了,则选择追击技能类似的范围(前排换后排,后排换前排)
            %#data_skill{targetSelect=TarSelect} = Skill,
            select_target(Skill,SrcPos);
        List ->
            List
    end.

%% @doc 获取敌方活着的玩家
enemy_alive(SrcPos) ->
    get_alive(SrcPos,false).

%% @doc 获取己方或者的玩家
self_alive(SrcPos) ->
    get_alive(SrcPos,true).

get_alive(SrcPos,IsSelf) ->
	SrcCamp= 
    case IsSelf of
        true ->
            ?sign(SrcPos);
        _ ->
            not ?sign(SrcPos)
    end,
    lists:foldl(fun(#fighter_info{pos=Pos},Acc) when abs(Pos) < 10 ->
                    case ?sign(Pos) =:= SrcCamp of
                        true  ->
                            [Pos|Acc];
                        _ ->
                            Acc
                    end;
				   (_,Acc) ->
						Acc
                end,[],?get_alives()).

%% @doc 技能目标选择,返回目标pos而不是Ger实例,只有在必要时才获取实例
%% 单一敌人
select_target(Skill,SrcPos) ->
	#data_skill{targetSelect=TarSelect, skillType=SkillType} = Skill,
	#unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(SrcPos),
	#unique_effect{target_add=TargetAddData} = UniqueBuffer,
	TargetAddR = 
		lists:foldl(fun(TargetAdd,AddAcc) ->case TargetAdd of
												{1,N} when SkillType == normal -> N+AddAcc;
												{2,N} when SkillType == unique -> N+AddAcc;
												{2,N} when SkillType == unique2 -> N+AddAcc;
												N when is_integer(N) -> N+AddAcc;
												_ -> AddAcc
											end 
					end,0,TargetAddData),
	select_target(TarSelect,TargetAddR,SrcPos,false).
select_target(Type,SrcPos,IsTrainer) ->
	select_target(Type,0,SrcPos,IsTrainer).

%% 暴怒选择己方除自己外的其他队友作为攻击对象,如果没有则还是选择从
%% 对面选择攻击对象
get_alive_list(Fun, SrcPos, IsTrainer) ->
    InRage = 
        case IsTrainer of
            true ->
                false;
            _ ->
                #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(SrcPos),
                #unique_effect{in_rage=InRageT} = UniqueBuffer,
                InRageT
        end,
    case InRage of
        true ->
            case lists:delete(SrcPos, erlang:apply(Fun, [-SrcPos])) of
                [] ->
                    erlang:apply(Fun, [SrcPos]);
                Any ->
                    Any
            end;
        _ ->
            erlang:apply(Fun, [SrcPos]) 
    end.
select_target(random_one,TargetAdd,SrcPos,IsTrainer) ->
	random_select_list(1+TargetAdd, get_alive_list(fun enemy_alive/1, SrcPos,IsTrainer));
select_target(random_two,TargetAdd,SrcPos,IsTrainer) ->
	random_select_list(2+TargetAdd, get_alive_list(fun enemy_alive/1, SrcPos,IsTrainer));
select_target(random_three,TargetAdd,SrcPos,IsTrainer) ->
	random_select_list(3+TargetAdd, get_alive_list(fun enemy_alive/1, SrcPos,IsTrainer));
select_target(own_random_one,TargetAdd,SrcPos,IsTrainer) ->
	random_select_list(1+TargetAdd, get_alive_list(fun self_alive/1, SrcPos,IsTrainer));
select_target(own_random_two,TargetAdd,SrcPos,IsTrainer) ->
	random_select_list(2+TargetAdd, get_alive_list(fun self_alive/1, SrcPos,IsTrainer));
select_target(own_random_three,TargetAdd,SrcPos,IsTrainer) ->
	random_select_list(3+TargetAdd, get_alive_list(fun self_alive/1, SrcPos,IsTrainer));
select_target({weakest,N},TargetAdd1,SrcPos,IsTrainer) ->
    List = get_alive_list(fun enemy_alive/1, SrcPos,IsTrainer),
    HpList = 
        lists:foldl(fun(E, Acc) ->
                        #ger{gerHp=GerHp} = ?get_ger(E),
                        [{GerHp,E}|Acc]
                 end,[],List),
    HpList2 = lists:sort(fun({HpA,_}, {HpB,_})-> HpA < HpB end,HpList),
	case IsTrainer of
		true ->	Weakest = 0;
		_ -> #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(SrcPos),
			#unique_effect{weakest=Weakest0,target_add=TargetAddData} = UniqueBuffer,
			 TargetAdd = lists:foldl(fun(X,TAAcc) when is_integer(X)-> X+TAAcc;
										(_,TAAcc) -> TAAcc
									 end, 0, TargetAddData),
			Weakest = Weakest0 + TargetAdd +TargetAdd1
	end,
    lists:foldl(fun({_,Pos},Acc) -> [Pos|Acc] end,[],lists:sublist(HpList2, N + Weakest));
%select_target(_TarSelect,TargetAdd,SrcPos,_IsTrainer) when TargetAdd >= 6 ->
%	enemy_alive(SrcPos);
select_target(TarSelect,TargetAdd,SrcPos,IsTrainer) ->
	FightOrder= get_fight_order(TarSelect,SrcPos,IsTrainer),
	Selected = select_target2(FightOrder),
	case IsTrainer of true -> Selected;
		_ -> select_add(TarSelect,TargetAdd,SrcPos,Selected)
	end.

select_add(own_self,_,_,Selected) -> Selected;
select_add(own_random_one,_,_,Selected) -> Selected;
select_add(own_random_two,_,_,Selected) -> Selected;
select_add(own_random_three,_,_,Selected) -> Selected;
select_add(_,0,_,Selected) -> Selected;
select_add(_,Add,SrcPos,Selected) ->
	Other = enemy_alive(SrcPos) -- Selected,
	Selected ++ random_select_list(Add,Other).


select_target2([]) ->
	[];
select_target2([TarList|FightOrder]) ->
	case select_alive(TarList) of
		[] ->
			select_target2(FightOrder);
		TarAlive->
			TarAlive
	end.

%% @doc 选择技能
select_skill(Ger,GerSkills) ->
	#ger{gerSp=GerSp} = Ger,
	if GerSp >= ?MAXSP ->
		   #gerSkill{unique=UniqueSkills} = GerSkills,
           {unique,data_skill:get(random_one_skill(UniqueSkills))};
	   true ->
		   #gerSkill{normal=NormalSkills} = GerSkills,
           {normal,data_skill:get(random_one_skill(NormalSkills))}
	end.

random_one_skill([])->
	0;
random_one_skill([H|_])->
    H.

%% @doc 初始化ger和动态buffer列表
init_fight_dict(GerList) ->
	lists:foreach(fun(#ger{gerBase=GerBase}=Ger) ->
						  #gerBase{gerPos=GerPos,gerTypeID=GerTypeID,gerQuality=GerQuality} = GerBase,
						  %erlang:put(GerPos,Ger),
						  ?set_ger(Ger),
						  erlang:put({dynamic_buffer,GerPos},#dynamic_buffer{}),
						  erlang:put({god,GerPos},{0,Ger}),
						  ?set_unique_buffer(GerPos,#unique_buffer{}),
						  ?set_damage_link(GerPos,{[],[],[]}),
						  ?set_ger_maimed(GerPos, []),
						  ?set_target_maimed(GerPos,[]),
						  ?set_ger_other_maimed(GerPos, []),
						  
						  UniqueHook1 = 
							  case data_unique_append:get(GerTypeID) of
								  undefined ->
									  #unique_trigger{};
								  List ->
									  get_unique_hook(List,GerPos,GerQuality,#unique_trigger{}) 
							  end,
					      #gerSkill{buff=QualityBuffSkillIDList} = ger_lib:get_ger_skills(GerTypeID,GerQuality),
						  %%在此处向精灵的必杀技能触发特效中添加上精灵第5阶段觉醒的触发技能
						  UniqueHook2 = add_awake_five_step_trigger(UniqueHook1,Ger,QualityBuffSkillIDList),
						  UniqueHook = add_gerDiamond_trigger(UniqueHook2,Ger),
						  ?set_unique_hook(GerPos, UniqueHook),
						  
						  ?set_pos_skinbuff(GerPos,role_skin:get_skinbuff_for_ger(get_skinbuff(GerPos),GerTypeID)) 
				  end,GerList).


%% FPos2  替补位,GerPos  被替补位
change_fight_dict(FPos2,GerPos)->
	#ger{gerBase=GerBase}=Ger = ?get_ger(FPos2),
	OldGer = ?get_ger(GerPos),
	?set_ger(Ger#ger{gerBase=GerBase#gerBase{gerPos=GerPos}}),
	erlang:put({dynamic_buffer,GerPos},erlang:get({dynamic_buffer,FPos2})),
	{TTimes,#ger{gerBase=GodGerBase}=InitGer} = ?get_god_skill(FPos2),
	erlang:put({god,GerPos}, {TTimes,InitGer#ger{gerBase=GodGerBase#gerBase{gerPos=GerPos}}}),
	?set_unique_buffer(GerPos,?get_unique_buffer(FPos2)),
	?set_damage_link(GerPos,{[],[],[]}),
	?set_ger_maimed(GerPos, []),
	?set_target_maimed(GerPos,[]),
	?set_ger_other_maimed(GerPos, []),
	?set_unique_hook(GerPos,?get_unique_hook(FPos2)),
	?set_pos_skinbuff(GerPos,?get_pos_skinbuff(FPos2)),
	erase({god,FPos2}),
	case OldGer of
		?undefined ->
			erase(FPos2),
			ignore;
		_ ->
			%#ger{gerBase=OldGerBase}=OldGer,
			%?set_ger(OldGer#ger{gerBase=OldGerBase#gerBase{gerPos=FPos2}})
			put(FPos2,OldGer)
	end,
	erlang:erase({reborn,GerPos}).
	
mark_reborn(DeadList) ->
	[begin
		 Pos2 = if Pos > 10 -> 10- Pos;
				   true -> 0- Pos
				end,
		 G=?get_ger(Pos2),
		 if is_record(G,ger) ->
				case G#ger.gerID of GerID -> ?set_reborn(Pos2);
					_ -> ?ERR("mark reborn info not match:~w",[{GerID,Pos,Pos2}])
				end;
			true ->
				?ERR("mark reborn info unknown:~w",[{GerID,Pos,Pos2}])
		 end
	 end||{GerID,Pos}<-DeadList].

%% @doc 记录god技能,计算精灵的属性
apply_attr_and_record_god() ->
    lists:foreach(fun(#fighter_info{pos=Pos,skill=#gerSkill{god=_GodSkill}}) ->
                        Ger = ?get_ger(Pos),
						record_ger_god_skill(Pos,Ger),
	                    Ger2 = ger_attr:add_enter_skill_attr(Ger,Ger#ger.gerExtra),
                        erlang:put(Pos,Ger2)
                    end,?get_alives()).

%% @doc 增加位置符号，在上面的单位位置符号为-1
add_pos_sign(DefenderList) ->
	[add_pos_sign2(Defender)||Defender<-DefenderList].
add_pos_sign2(#ger{gerBase=#gerBase{gerPos=GerPos}=GerBase}=Defender) ->
	Defender#ger{gerBase=GerBase#gerBase{gerPos=-GerPos}};
%%由于155升级到160添加了附魔属性，所以此处会出现匹配失败的情况，需要直接添加上附魔属性以及修改add_attr
add_pos_sign2(OldGer)->
    Defender=ger_attr:transOldGer2NewGer(OldGer),
	#ger{gerBase=#gerBase{gerPos=GerPos}=GerBase}=Defender,
	Defender#ger{gerBase=GerBase#gerBase{gerPos=-GerPos}}.

%% 获得精灵的绝技附加效果
get_unique_hook([],_,_,Acc) ->
    Acc;
get_unique_hook([{Need,AppendID}|T],Pos,GerQuality,#unique_trigger{t2=T2,t4=T4}=Acc) ->
    case Need > GerQuality of 
        true ->
            Acc;
        _ ->
            case data_unique_effect:get(AppendID) of
                undefined ->
                    get_unique_hook(T,Pos,GerQuality,Acc);
                #data_unique_effect{opportune=Opportune,condition=Condition,apply_type=ApplyType,args=Args,replaceID=ReplaceID} ->
                    NewAcc = 
                        case Opportune of
                            8 ->
                                Acc;
                            2 ->
                                TPos = Condition + 2,
                                Older = lists:delete(ReplaceID, erlang:element(TPos, T2)),
                                Acc#unique_trigger{t2=erlang:setelement(TPos,T2,[AppendID|Older])};
                            4 ->
                                TPos = Condition + 2,
                                Older = lists:delete(ReplaceID, erlang:element(TPos, T4)),
                                Acc#unique_trigger{t4=erlang:setelement(TPos,T4,[AppendID|Older])};
							20 ->
								#unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
								UniqueEffect2 = 
                                    case ApplyType of
                                        64 -> UniqueEffect#unique_effect{is_cp=0}
                                    end,
                                ?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2}),
								Older = lists:delete(ReplaceID, erlang:element(21, Acc)),
                                erlang:setelement(21, Acc, [AppendID|Older]);
                            99 ->
                                #unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
								TargetAdd = UniqueEffect#unique_effect.target_add,
                                UniqueEffect2 = 
                                    case ApplyType of
                                        17 -> UniqueEffect#unique_effect{sleep=erlang:hd(Args)};
                                        19 -> UniqueEffect#unique_effect{weak=Args};
                                        20 -> UniqueEffect#unique_effect{weakest=erlang:hd(Args)};
                                        28 -> UniqueEffect#unique_effect{confuse_inc=erlang:hd(Args)};
                                        34 -> UniqueEffect#unique_effect{morph_sleep_critic=erlang:hd(Args)};
										40 -> [Ty,Ra]=Args,UniqueEffect#unique_effect{holy_kill={Ty,Ra}};
										46 -> UniqueEffect#unique_effect{dead_mark=erlang:hd(Args)};
										55 -> UniqueEffect#unique_effect{target_add=[erlang:hd(Args)|TargetAdd]};
										65 -> UniqueEffect#unique_effect{con_attack=true}
                                    end,
                                ?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2}),
                                Acc;
                            N ->
                                TPos = N + 1,
                                Older = lists:delete(ReplaceID, erlang:element(TPos, Acc)),
                                erlang:setelement(TPos,Acc,[AppendID|Older])
                        end,
                    #unique_buffer{use=MUniqueEffect} = MUniqueBuffer =  ?get_unique_buffer(Pos),
                    #unique_effect{morph_has_status=MHS} = MUniqueEffect,
                    MUniqueEffect2 = 
                        case ApplyType of
                            8 ->
                                Max_c = case lists:keyfind(max_c, 1, Args) of false -> ?UNIQURE_APPEND_LIMIT; {_,MaxCC} -> MaxCC end, 
                                MUniqueEffect#unique_effect{maxCount = Max_c};
                            35 ->
                                MUniqueEffect#unique_effect{morph_status=?ger_morph_status_sing, morph_has_status=[?ger_morph_status_sing|lists:delete(?ger_morph_status_sing, MHS)]};
                            36 ->
                                MUniqueEffect#unique_effect{morph_has_status=[?ger_morph_status_dance|lists:delete(?ger_morph_status_dance,MHS)]};
                            _ ->
                                MUniqueEffect
                        end,
                    MUniqueBuffer2 = MUniqueBuffer#unique_buffer{use=MUniqueEffect2},
                    ?set_unique_buffer(Pos, MUniqueBuffer2),
                    get_unique_hook(T,Pos,GerQuality,NewAcc)
            end
    end.

sort_fighters(Fighters) ->
	Fighters2 = [F#fighter_info{speed=(?get_ger(Pos))#ger.gerSpeed}||#fighter_info{pos=Pos}=F<-Fighters],
	lists:sort(fun(#fighter_info{pos=Pos1,speed=Speed1},#fighter_info{pos=Pos2,speed=Speed2})->
					   if Speed1 > Speed2 -> true;
						  Speed1 == Speed2 -> 
							  if abs(Pos1) < abs(Pos2) -> true;
								 abs(Pos1) == abs(Pos2) andalso Pos1 > Pos2 -> true;
								 true -> false
							  end;
						  true -> false
					   end
			   end , Fighters2).

%% 技能的目标个数
skill_target_num(single) ->	1;
skill_target_num(single_back) ->1;
skill_target_num(front_row) ->	3;
skill_target_num(back_row) ->	3;
skill_target_num(array) ->	2;
skill_target_num(random_one) ->1;
skill_target_num(random_two) ->	2;
skill_target_num(random_three) ->	3;
skill_target_num(all) ->6;
skill_target_num(own_self) ->1;
skill_target_num(own_single) ->1;
skill_target_num(own_front_row) ->3;
skill_target_num(own_back_row) ->3;
skill_target_num(own_all) ->6;
skill_target_num(own_array) ->2;
skill_target_num(own_single_back) ->1;
skill_target_num(own_random_one) -> 1;
skill_target_num(own_random_two) ->2;
skill_target_num(own_random_three) ->3;
skill_target_num(cross) -> 3;
skill_target_num(own_cross) -> 3;
skill_target_num({weakest,_}) -> 1.

%% @计算战斗力使用
select_posList(TarSelect,Pos,PosList) ->
	SelectList = get_fight_order(TarSelect,Pos,false),
	util:fun_find(fun(Select) ->
						  lists:any(fun(D) -> lists:member(D,PosList) end,Select)
				  end,SelectList).

%% 静态化目标选择逻辑.暴怒选择己方除自己外的其他队友作为攻击对象
%% 如果没有则还是选择对面进行攻击
get_fight_order(TarSelect,Pos, IsTrainer) ->
	if Pos > 10 ->
		   L = get_fight_order2(TarSelect,Pos-10,IsTrainer,Pos),
		   [[begin if L2 > 0 -> L2 + 10; true -> L2 - 10 end end||L2<-L1]||L1<-L];
	   Pos < -10 ->
		   L = get_fight_order2(TarSelect,Pos+10,IsTrainer,Pos),
		   [[begin if L2 > 0 -> L2 + 10; true -> L2 - 10 end end||L2<-L1]||L1<-L];
	   true ->
		   get_fight_order2(TarSelect,Pos,IsTrainer,Pos)
	end.
get_fight_order2(TarSelect,Pos,IsTrainer,Pos2) ->
    InRage = 
        case IsTrainer of
            true ->
                false;
            _ ->
                #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(Pos2),
                #unique_effect{in_rage=InRageT} = UniqueBuffer,
                InRageT
        end,
    case InRage of
        true ->
            case filter_fight_order(Pos, data_fight_order:get({TarSelect,-Pos})) of
                [] ->
                    data_fight_order:get({TarSelect,Pos});
                Any ->
                    Any
            end;
        _ ->
            data_fight_order:get({TarSelect,Pos}) 
    end.

%% @doc 从一个列表中随机抽取N个,顺序随机,N可以超过界限
random_select_list(N,List) ->
    Length = erlang:length(List),
    case Length of
        0 ->
            [];
        % FIX此处如果N为0,List长度为1的情况下将返回错误
        % 1 ->
        %     List;
        L ->
	        random_list2(List,erlang:min(N,L),L,[]) 
    end.

random_list2(_List,0,_Length,Result) ->
    Result;
random_list2(List,N,Length,Result) ->
	Rand = random:uniform(Length),
	{value,Select,Rest} = util:nth_take(Rand,List),		   
	random_list2(Rest,N - 1,Length - 1,[Select|Result]).

%% @doc 从(lower...Higher)中随机出一个浮点数出来
random_float(Lower,Lower) ->
	Lower;
random_float(Lower,Higher) ->
    random:uniform() * (Higher - Lower) + Lower.

%% 计算精灵的奥义护盾值
calc_profound_hp(GerAttr) ->
    #gerAttr{gerProMean=ProMean,gerProMeanAddtion=ProMeanAddtion,gerAttckToProMean=AttackToProMean,
                gerHpToProMean=HpToProMean,gerHpMax=HpMax,gerAttack=Attack} = GerAttr,
    BaseValue = ProMean + HpMax * HpToProMean / 10000 + Attack * AttackToProMean / 10000,
    erlang:trunc(BaseValue * (1 + ProMeanAddtion / 10000)).

%% 登场的hook
enter_hook(ActionList) ->
    %% 选出两边第一个出战的精灵来释放技能
    [Pos1|_] = self_alive(1),
    [Pos2|_] = self_alive(-1),
    TriggerList1 = erlang:get({?ENTHER_HOOK,Pos1 > 0}),
    ActionList1 = do_trigger_list(TriggerList1,Pos1,[],ActionList),
    TriggerList2 = erlang:get({?ENTHER_HOOK,Pos2 > 0}),
    ActionList2 = do_trigger_list(TriggerList2,Pos2,[],ActionList1),
    %% 第一回也要触发新回合的hook
    new_loop_hook(Pos1,Pos2,ActionList2).  

%% 每回合开始的hook,应策划需求,改为两边轮流放
get_new_loop_hook(Pos) ->
    case erlang:get({?NEW_LOOP_HOOK, Pos > 0}) of
        undefined ->
            [];
        Any ->
            lists:sort(fun talent_skill_sort/2, Any)
    end.
            
new_loop_hook(ActionList) ->
    %% 选出两边第一个出战的精灵来释放技能
    [Pos1|_] = self_alive(1),
    [Pos2|_] = self_alive(-1),
    new_loop_hook(Pos1,Pos2,ActionList).

new_loop_hook(Pos1,Pos2,ActionList) ->
    TriggerList1 = get_new_loop_hook(Pos1),
    TriggerList2 = get_new_loop_hook(Pos2),
    new_loop_hook(TriggerList1,Pos1,TriggerList2,Pos2,ActionList,1).

new_loop_hook([],_,TriggerList2,Pos2,ActionList,_Pos) ->
    do_trigger_list(TriggerList2,Pos2,[],ActionList);
new_loop_hook(TriggerList1,Pos1,[],_,ActionList,_Pos) ->
    do_trigger_list(TriggerList1,Pos1,[],ActionList);
new_loop_hook([H|T],Pos1,TriggerList2,Pos2,ActionList,1) ->
    NewActionAcc = do_trigger_list([H],Pos1,[],ActionList),
    new_loop_hook(T,Pos1,TriggerList2,Pos2,NewActionAcc,2);
new_loop_hook(TriggerList1,Pos1,[H|T],Pos2,ActionList,2) ->
    NewActionAcc = do_trigger_list([H],Pos2,[],ActionList),
    new_loop_hook(TriggerList1,Pos1,T,Pos2,NewActionAcc,1).
    
before_attack_hook(SrcPos,ActionList) ->
    TriggerList = erlang:get({?BEFORE_ATTACK_HOOK,SrcPos > 0}),
    do_trigger_list(TriggerList,SrcPos,[],ActionList).

after_attack_hook(SrcPos,ActionList) ->
    TriggerList = erlang:get({?AFTER_ATTACK_HOOK,SrcPos > 0}),
    do_trigger_list(TriggerList,SrcPos,[],ActionList).

reborn_hook(GerPos,ActionList) ->
    TriggerList = erlang:get({?REBORN_HOOK,GerPos > 0}),
    do_trigger_list(TriggerList,GerPos,[],ActionList).

%% SrcPos : 攻击的精灵 GerPos:被攻击死亡的精灵
killed_hook(SrcPos,GerPos) ->
    Attacker = 
        case SrcPos =:= 0 of
            true ->
                -GerPos;
            _ ->
                SrcPos
        end,
    TriggerList = erlang:get({?KILLED_HOOK,Attacker > 0}),
    do_trigger_list(TriggerList,Attacker,[],[]).

%% GerPos : 被攻击死亡的精灵 SrcPos:攻击的精灵
dead_hook(GerPos,SrcPos,ActionList) ->
    TriggerList = 
        case SrcPos =:= 0 of
            true ->
                [];
            _ ->
                erlang:get({?DEAD_HOOK,GerPos > 0}) 
        end,
    do_trigger_list(TriggerList,GerPos,[SrcPos],ActionList).

attack_hook(SrcPos,TarPos,ActionList) ->
    TriggerList = erlang:get({?ATTACK_HOOK,SrcPos > 0}),
    do_trigger_list(TriggerList,SrcPos,[TarPos],ActionList).

%%触发天赋技能:ID列表,攻击者,选中的目标(没有则为[]),出手队列,ActionAcc
do_trigger_list(undefined,_,_,ActionAcc) ->
    ActionAcc;

do_trigger_list([],_,_,ActionAcc) ->
    ActionAcc;

do_trigger_list([H|T],SrcPos,TargetList,ActionAcc) ->
    NewActionAcc = do_trigger(H,SrcPos,TargetList,ActionAcc),
    do_trigger_list(T,SrcPos,TargetList,NewActionAcc).

%% TODO 对于无敌精灵的动态天赋属性,先暂时不处理,因为目前动态天赋属性都是正面buffer
%% 另外一个原因就是动态天赋属性是动态的,无敌状态也是动态的,涉及的问题较多,暂时不处理
do_trigger({ID,_,_,_} = TalenInfo,SrcPos,TargetList,ActionAcc) ->
    #data_talent_trigger{condition=Condition,check_target=CheckTarget,
                            apply_target=ApplyTarget,check_op=CheckOp,arg1 = Arg1,
                            arg2=Arg2,is_dynamic=IsDynamic,actionID=ActionID} = data_talent_skill:get(ID),
   case check_condition(Condition,SrcPos,TargetList,CheckTarget,CheckOp,Arg1,Arg2) of
        false ->
            %% 如果是动态属性,不满足条件时需要去掉加成
            case IsDynamic of
                true ->
                    unapply_target(TalenInfo,SrcPos,TargetList,ApplyTarget),
                    ActionAcc;
                _ ->
                    ActionAcc
            end;
        _ ->
            apply_target(TalenInfo,SrcPos,TargetList,ApplyTarget,IsDynamic,ActionID,ActionAcc)
    end.

check_condition(0,_,_,_,_,_,_) ->
    true;
check_condition(1001,SrcPos,TargetList,CheckTarget,CheckOp,Arg1,_) ->
    case CheckTarget of
        0 ->
            check_hp(SrcPos,CheckOp,Arg1);
        7 ->
            case TargetList =:= [] of
                true ->
                    false;
                _ ->
                    [Target|_] = TargetList,
                    check_hp(Target,CheckOp,Arg1) 
            end;
        _ ->
            false
    end;
check_condition(2001,SrcPos,_,CheckTarget,CheckOp,Arg1,_) ->
    case CheckTarget of
        1 ->
            Value = erlang:length(self_alive(SrcPos)),
            check_op(Value,CheckOp,Arg1);
        4 ->
            Value = erlang:length(enemy_alive(SrcPos)),
            check_op(Value,CheckOp,Arg1);
        _ ->
            false
    end.

check_hp(Pos,CheckOp,Arg1) ->
    #ger{gerHp=GerHp,gerAttr =_=#gerAttr{gerHpMax=GerHpMax}} = erlang:get(Pos),
    Value = erlang:trunc(GerHp * 100 / GerHpMax),
    check_op(Value,CheckOp,Arg1).

check_op(Value,CheckOp,Arg1) ->
    case CheckOp of
        0 ->
            Value < Arg1;
        1 ->
            Value > Arg1;
        2 ->
            Value =:= Arg1
    end.

%% 获得施加对象
get_applylist_attr(AddAttr,SubAttr,SrcPos,ApplyTarget,TargetList,DeadMark,AllHandled,{InRage,GerPos,Acount}) ->
    case ApplyTarget of
        0 ->
            {[SrcPos],AddAttr};
        1 ->
            {self_alive(SrcPos),AddAttr};
        2 ->
            {get_front(SrcPos,true),AddAttr};
        3 ->
            {get_back(SrcPos,true),AddAttr};
        4 ->
            {enemy_alive(SrcPos),SubAttr};
        5 ->
            {get_front(SrcPos,false),SubAttr};
        6 ->
            {get_back(SrcPos,false),SubAttr};
        7 ->
             %%这里只取被攻击方(正在被打的那个),所以外部传递时,需要把被打的那个放在表头
             case TargetList of
                [] ->
                    {[],SubAttr};
                [H|_] ->
                    {[H],SubAttr} 
             end;
        {8,N} ->
            {random_select_list(N,self_alive(SrcPos)),AddAttr};
        {9,N} ->
            %% 这里过先滤掉无敌的
            {random_select_list(N,filter_invincible(enemy_alive(SrcPos))),SubAttr};
        10 ->
            {get_cross(SrcPos,true),AddAttr};
        11 ->
            {get_cross(SrcPos,false),SubAttr};
        {12,WeightList} ->
            RN = util:random_one_from_weigh_list(WeightList),
            %% 这里先过滤掉无敌的
            {random_select_list(RN,filter_invincible(enemy_alive(SrcPos))),SubAttr};
        13 ->
            {TargetList,SubAttr};
        14 ->
            {get_back_one(SrcPos,false), SubAttr};
		15 ->
			RN = 1 + DeadMark,
			{random_select_list(RN,filter_invincible(enemy_alive(SrcPos))),SubAttr};
		{16,ElePos,BaseID} ->
			case AllHandled of 0 -> {[],SubAttr};
				_ -> UnHandledList = element(ElePos+1,AllHandled),
			{[CPos||{CBaseID,CPos,_}<-UnHandledList, CBaseID == BaseID andalso lists:member(CPos, TargetList)],SubAttr}
			end;
		17 ->
             %%这里只取被攻击方(正在被打的那个),所以外部传递时,需要把被打的那个放在表头
			case TargetList of
				[] ->
					{[],SubAttr};
				[H|_] ->
					EnermyAlive = enemy_alive(GerPos),
                    SelfAlive = self_alive(GerPos),
                    #unique_buffer{use=#unique_effect{target_add=TargetAddData}} = ?get_unique_buffer(GerPos),
                    TargetAdd =  lists:foldl(fun(TargetAdd,AddAcc) ->case TargetAdd of {2,N} -> N+AddAcc; _ -> AddAcc end end,0,TargetAddData),
                    case InRage of 
                        true ->
							H1 = case lists:member(H, SelfAlive) of true -> H; _ -> hd(SelfAlive) end,
							List1 = lists:delete(H1,lists:delete(GerPos,SelfAlive)),
							List2 = filter_invincible(List1),
							List1Length = length(List1),
							LengthDiff = Acount+1-List1Length,
							List4 = [H1|random_select_list(Acount+1,List2)],
							if LengthDiff > 0 -> 
								   List3 = random_select_list(LengthDiff ,filter_invincible(EnermyAlive)),
								   {List3++List4, SubAttr};
							   true -> {List4,SubAttr}
							end;
						_ ->H1 = case lists:member(H, EnermyAlive) of true -> H; _ -> hd(EnermyAlive) end, 
							{[H1|random_select_list(Acount+1+TargetAdd,filter_invincible(lists:delete(H1,EnermyAlive)))],SubAttr}
					end
			end
	end.

unapply_target({ID,_,AddAttr,SubAttr},SrcPos,TargetList,ApplyTarget) ->
    {ApplyList,UseAttr} = get_applylist_attr(AddAttr,SubAttr,SrcPos,ApplyTarget,TargetList,0,0,{0,0,0}),
    %% 当条件不满足时,消去精灵的动态buffer
    lists:foreach(fun(GerPos) ->
                    #dynamic_buffer{attr=Attr,buffers=Buffers} = erlang:get({dynamic_buffer,GerPos}),
                    case lists:keytake(ID,1,Buffers) of
                        false ->
                            ignore;
                        {value,{_,UseAttr},Buffers2} ->
                            OppAttr = 
                                case erlang:is_record(UseAttr,add_attr) of
                                    true ->
                                        erlang:setelement(1,UseAttr,sub_attr);
                                    _ ->
                                        erlang:setelement(1,UseAttr,add_attr)
                                end,
                            erlang:put({dynamic_buffer,GerPos},#dynamic_buffer{attr=ger_attr:append_add_attr(Attr,OppAttr),buffers=Buffers2})
                    end
                end,ApplyList).

apply_target({ID,Lv,AddAttr,SubAttr},SrcPos,TargetList,ApplyTarget,IsDynamic,ActionID,ActionAcc) ->
    {ApplyList,UseAttr} = get_applylist_attr(AddAttr,SubAttr,SrcPos,ApplyTarget,TargetList,0,0,{0,0,0}),
    case IsDynamic of
        true ->
            %% 动态的buffer
            lists:foreach(fun(GerPos) ->
                            #dynamic_buffer{attr=Attr,buffers=Buffers} = erlang:get({dynamic_buffer,GerPos}),
                            case proplists:get_value(ID,Buffers) of
                                undefined ->
                                    erlang:put({dynamic_buffer,GerPos},#dynamic_buffer{attr=ger_attr:append_add_attr(Attr,UseAttr),buffers=[{ID,UseAttr}|Buffers]});
                                _ ->
                                    %% buffer不叠加
                                    ignore
                            end
                        end,ApplyList),
            ActionAcc;
        _ ->
            %% 直接加到精灵身上的buffer
            SpInc = calc_talent_inc_sp(ActionID,Lv),
            lists:foldl(fun(GerPos,ActionAcc2) ->
                            Ger = ?get_ger(GerPos),
                            case add_talent_skill_attr(Ger,UseAttr,SpInc) of
                                invinciblep ->
                                    ActionAcc2;
                                {NewGer,HpInc} ->
                                    erlang:put(GerPos,NewGer),
                                    case calc_actionID_value(ActionID,UseAttr,SpInc,HpInc) of
                                        false ->
                                            ActionAcc2;
                                        Value ->
                                            [?action(ActionID,GerPos,[],Value,0,?STATE_TALENT)|ActionAcc2]
                                    end
                            end
                        end,ActionAcc,ApplyList) 
    end.

get_front(SrcPos,IsSelf) ->
    case IsSelf =:= (SrcPos > 0) of
        true ->
            select_alive([1,2,3]);
        _ ->
            select_alive([-1,-2,-3]) 
    end.

get_back(SrcPos,IsSelf) ->
    case IsSelf =:= (SrcPos > 0) of
        true ->
            select_alive([4,5,6]);
        _ ->
            select_alive([-4,-5,-6])
    end.

get_back_one(SrcPos,IsSelf) ->
    Back = get_back(SrcPos, IsSelf),
    case Back of
        [] ->
            [];
        [H|_] ->
            [H]
    end.

get_cross(SrcPos,IsSelf) ->
    AbsPos = abs(SrcPos),
    List = 
        case AbsPos of
            3 ->
                [2,6];
            4 ->
                [1,5];
            _ ->
                [AbsPos - 1,AbsPos + 1,AbsPos - 3,AbsPos + 3]
        end,
    case IsSelf =:= (SrcPos > 0) of
        true ->
            select_alive(List);
        _ ->
            select_alive([-X || X <- List])
    end.

%% 初始化天赋技能 
init_talent_hook(TalentListAll,IsAttack) ->
    {TalentList,TrainerBattleList} = 
        lists:foldl(fun({TriggerID,Lv},{Acc1,Acc2}) -> {[{TriggerID,Lv}|Acc1],Acc2};
                       ({TagID,Lv,Rank},{Acc1,Acc2}) -> {Acc1,[{TagID,Lv,Rank}|Acc2]}
                    end, {[],[]}, TalentListAll),
    init_talent_hook2(TalentList,IsAttack),
    TrainerAction = init_trainer_battle(TrainerBattleList,IsAttack),
    TrainerAction.

init_trainer_battle(List,IsAttack) ->
    lists:foldl(fun({TalentID,Lv,Rank},Acc) -> apply_trainer_battle(TalentID,Lv,Rank,IsAttack)++Acc end, [], List).

apply_trainer_battle(TagID,Lv,SkillRank,IsAttack) when TagID == 3 ->
    {{B1,B2},Buff1} = data_trainer_battle:get({skill,TagID,SkillRank}),
    {R,V} = {B1*Lv+Buff1,B2},
    SrcPos = if IsAttack -> 1; true -> -1 end,
    Self = self_alive(SrcPos),
    [begin 
         #unique_buffer{use=Use} = UniqueBuffer = ?get_unique_buffer(Pos),
         #unique_effect{superCrit={A,B}}=Use,
         Use2 = Use#unique_effect{superCrit={A+R,B+V}},
         UniqueBuffer2 = UniqueBuffer#unique_buffer{use=Use2},
         ?set_unique_buffer(Pos,UniqueBuffer2)
     end||Pos<-Self],
    [];
apply_trainer_battle(TagID,Lv,SkillRank,IsAttack) when TagID == 4 ->
    {{B1,_},Buff1} = data_trainer_battle:get({skill,TagID,SkillRank}),
    R = B1*Lv+Buff1,
    SrcPos = if IsAttack -> 1; true -> -1 end,
    Self = self_alive(SrcPos),
    [begin 
         #unique_buffer{use=Use} = UniqueBuffer = ?get_unique_buffer(Pos),
         #unique_effect{pm_dmg_dec=Dec}=Use,
         Use2 = Use#unique_effect{pm_dmg_dec=R+Dec},
         UniqueBuffer2 = UniqueBuffer#unique_buffer{use=Use2},
         ?set_unique_buffer(Pos,UniqueBuffer2)
     end||Pos<-Self],
    [];
apply_trainer_battle(TagID,Lv,SkillRank,IsAttack) when TagID == 5 ->
    {{B1,B2,B3},Buff1} = data_trainer_battle:get({skill,TagID,SkillRank}),
    Buf = {B1,B2,B3*Lv + Buff1},
    SrcPos = if IsAttack -> -1; true -> 1 end,
    put({trainer_battle_speed, SrcPos},{Buf,[]}),
        [];
apply_trainer_battle(TagID,Lv,SkillRank,IsAttack) when TagID == 6 ->
    {{B1,EffectID},Buff1} = data_trainer_battle:get({skill,TagID,SkillRank}),
    Ratio = (B1*Lv+Buff1) / 100,
    SrcPos = if IsAttack -> 1; true -> -1 end,
    Self = self_alive(SrcPos),
    [begin 
         #unique_buffer{use=Use} = UniqueBuffer = ?get_unique_buffer(Pos),
         Use2 = Use#unique_effect{skill_ra_up=[{[EffectID],Ratio}]},
         UniqueBuffer2 = UniqueBuffer#unique_buffer{use=Use2},
         ?set_unique_buffer(Pos,UniqueBuffer2),
         TarUniqueHook = ?get_unique_hook(Pos),
         TarUniqueHook2 = TarUniqueHook#unique_trigger{t25=[EffectID]},
         ?set_unique_hook(Pos,TarUniqueHook2)
     end||Pos<-Self],
        [];
apply_trainer_battle(_,_,_,_) -> [].

init_talent_hook2(TalentList,IsAttack)->
    InfoList = 
        lists:foldl(fun({TalentID,Lv},Acc) ->
                     #data_talent{trigger_type=TriggerID,add=AddAttr,sub=SubAttr} = data_talent:get(TalentID + Lv),
                     %% 0代表没有触发ID
                     case TriggerID =:= 0 of
                        true ->
                            Acc;
                        _ ->
                            #data_talent_trigger{opportune=Opportune} = data_talent_skill:get(TriggerID),
                            case lists:keytake(Opportune,1,Acc) of
                                false ->
                                    [{Opportune,[{TriggerID,Lv,AddAttr,SubAttr}]}|Acc];
                                {value,{_,List},Rest} ->
                                    [{Opportune,[{TriggerID,Lv,AddAttr,SubAttr}|List]} | Rest]
                            end
                    end
                end,[],TalentList),
    lists:foreach(fun({Opportune,TriggerList}) -> erlang:put({Opportune,IsAttack},TriggerList) end,InfoList).

%% 获得客户端的技能actionID和值
calc_actionID_value(ActionID,UseAttr,SpInc,HpInc) ->
    case ActionID of
        0 ->
            %% 0表示不发给客户端
            false;
        101 ->
            HpInc;
        102 ->
            case erlang:is_record(UseAttr,add_attr) of
                true ->
                    UseAttr#add_attr.gerPhyDefBite bsl 32 bor UseAttr#add_attr.gerMagDefBite;
                _ ->
                    (-UseAttr#sub_attr.gerPhyDefBite) bsl 32 bor (-UseAttr#sub_attr.gerMagDefBite)
            end;
        103 ->
            SpInc;
        113 ->
            SpInc;
        104 ->
            case erlang:is_record(UseAttr,add_attr) of
                true ->
                    UseAttr#add_attr.gerAbsorb;
                _ ->
                    -UseAttr#sub_attr.gerAbsorb
            end;
        105 ->
            case erlang:is_record(UseAttr,add_attr) of
                true ->
                    UseAttr#add_attr.gerDamageBack;
                _ ->
                    -UseAttr#sub_attr.gerDamageBack
            end
    end.

%% 给gerAttr加属性
add_talent_to_gerAttr(GerAttr,AddAttr) when is_record(AddAttr,add_attr) ->
	#gerAttr{gerAttack=GerAttack}=GerAttr,
	GerAttack2 = trunc((GerAttack+AddAttr#add_attr.gerAttack)*(10000+AddAttr#add_attr.gerAttackAddtion)/10000),
	GerAttr#gerAttr{
					gerAttack       	= GerAttack2 
				   ,gerCritic       	= GerAttr#gerAttr.gerCritic       		+  AddAttr#add_attr.gerCritic            			
				   ,gerCriticReduce 	= GerAttr#gerAttr.gerCriticReduce 		+  AddAttr#add_attr.gerCriticReduce  		
				   ,gerDoom         	= GerAttr#gerAttr.gerDoom         		+  AddAttr#add_attr.gerDoom                 			
				   ,gerMiss         	= GerAttr#gerAttr.gerMiss         		+  AddAttr#add_attr.gerMiss            			
				   ,gerAbsorb       	= GerAttr#gerAttr.gerAbsorb      	    +  AddAttr#add_attr.gerAbsorb  
				   ,gerDamageBack   	= GerAttr#gerAttr.gerDamageBack         +  AddAttr#add_attr.gerDamageBack  
				   ,gerReel         	= GerAttr#gerAttr.gerReel         		+  AddAttr#add_attr.gerReel               			
				   ,gerReelReduce   	= GerAttr#gerAttr.gerReelReduce   		+  AddAttr#add_attr.gerReelReduce    			
				   ,gerPhyDefBite   	= GerAttr#gerAttr.gerPhyDefBite   		+  AddAttr#add_attr.gerPhyDefBite    			
				   ,gerPhyDef       	= GerAttr#gerAttr.gerPhyDef       		+  AddAttr#add_attr.gerPhyDef            			
				   ,gerMagDefBite   	= GerAttr#gerAttr.gerMagDefBite   		+  AddAttr#add_attr.gerMagDefBite    			
				   ,gerMagDef       	= GerAttr#gerAttr.gerMagDef       		+  AddAttr#add_attr.gerMagDef       
				   ,gerProMean      	= GerAttr#gerAttr.gerProMean            +  AddAttr#add_attr.gerProMean      			
				   ,gerProMeanAddtion   = GerAttr#gerAttr.gerProMeanAddtion     +  AddAttr#add_attr.gerProMeanAddtion      			
				   ,gerAttckToProMean   = GerAttr#gerAttr.gerAttckToProMean     +  AddAttr#add_attr.gerAttckToProMean       			
				   ,gerHpToProMean      = GerAttr#gerAttr.gerHpToProMean        +  AddAttr#add_attr.gerHpToProMean       	
				};

add_talent_to_gerAttr(GerAttr,SubAttr) ->
	#gerAttr{gerAttack=GerAttack}=GerAttr,
	GerAttack2 = max(0,trunc((GerAttack-SubAttr#sub_attr.gerAttack)*(10000-SubAttr#sub_attr.gerAttackAddtion)/10000)),
    GerAttr#gerAttr{
                     gerAttack            = GerAttack2 
                    ,gerCritic           = max(0,GerAttr#gerAttr.gerCritic               -  SubAttr#sub_attr.gerCritic)
                    ,gerCriticReduce     = max(0,GerAttr#gerAttr.gerCriticReduce         -  SubAttr#sub_attr.gerCriticReduce)
                    ,gerDoom             = max(0,GerAttr#gerAttr.gerDoom                 -  SubAttr#sub_attr.gerDoom)
                    ,gerMiss             = max(0,GerAttr#gerAttr.gerMiss                 -  SubAttr#sub_attr.gerMiss)
				    ,gerAbsorb       	= max(0,GerAttr#gerAttr.gerAbsorb      	 - SubAttr#add_attr.gerAbsorb)
				    ,gerDamageBack   	= max(0,GerAttr#gerAttr.gerDamageBack      - SubAttr#add_attr.gerDamageBack)
                    ,gerReel             = max(0,GerAttr#gerAttr.gerReel                 -  SubAttr#sub_attr.gerReel)
                    ,gerReelReduce       = max(0,GerAttr#gerAttr.gerReelReduce           -  SubAttr#sub_attr.gerReelReduce)
                    ,gerPhyDefBite       = max(0,GerAttr#gerAttr.gerPhyDefBite           -  SubAttr#sub_attr.gerPhyDefBite)
                    ,gerPhyDef           = max(0,GerAttr#gerAttr.gerPhyDef               -  SubAttr#sub_attr.gerPhyDef)
                    ,gerMagDefBite       = max(0,GerAttr#gerAttr.gerMagDefBite           -  SubAttr#sub_attr.gerMagDefBite)
                    ,gerMagDef           = max(0,GerAttr#gerAttr.gerMagDef               -  SubAttr#sub_attr.gerMagDef)
                    ,gerProMean          = max(0,GerAttr#gerAttr.gerProMean              - SubAttr#sub_attr.gerProMean)
                    ,gerProMeanAddtion   = max(0,GerAttr#gerAttr.gerProMeanAddtion       - SubAttr#sub_attr.gerProMeanAddtion)
                    ,gerAttckToProMean   = max(0,GerAttr#gerAttr.gerAttckToProMean       - SubAttr#sub_attr.gerAttckToProMean)
                    ,gerHpToProMean      = max(0,GerAttr#gerAttr.gerHpToProMean          - SubAttr#sub_attr.gerHpToProMean)
            	}.

%% 添加天赋技能
add_talent_skill_attr(Ger,AddAttr,Sp)  when is_record(AddAttr,add_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp,gerSp=GerSp,gerBase=#gerBase{gerPos=GerPos,gerTypeID=SrcGerTypeID}}=Ger,
	#gerAttr{gerHpMax=GerHpMax,gerSpMax=GerSpMax}=GerAttr,
    #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(GerPos),
    #unique_effect{invinciblep=Invinciblep,cure_sub=CurSub,cure_add=CurAdd} = UniqueBuffer,
    case Invinciblep of
        true ->
            invinciblep;
        _ ->
            %% 回复的血量使用血量上限来计算
            HpIncT = trunc(GerHpMax*AddAttr#add_attr.gerHpMaxAddtion/10000),	
        	CurAdd2 = calc_tr_ctl_add(CurAdd,SrcGerTypeID,GerPos),
            %% 治疗减益
            HpInc = 
                case CurSub + CurAdd of
                    0 ->
                        HpIncT;
                    _ ->
                        erlang:trunc(HpIncT * (100 - CurSub+CurAdd2/100) / 100)
                end,
        	GerHp2= GerHp + HpInc,
            GerAttr2 = add_talent_to_gerAttr(GerAttr,AddAttr),
        	%% 当前血量不能超过血量上限
        	GerHp3 = 
                case GerHp2 >= GerHpMax of
                    true ->
                        GerHpMax;
                    _ ->
                        GerHp2
                end,
            {Ger#ger{gerAttr=GerAttr2,gerHp=GerHp3,gerSp=min(GerSp+Sp,GerSpMax)},HpInc} 
    end;
add_talent_skill_attr(Ger,SubAttr,Sp) when is_record(SubAttr,sub_attr)->
	#ger{gerAttr=GerAttr,gerHp=GerHp,gerSp=GerSp,gerBase=#gerBase{gerPos=GerPos}}=Ger,
    #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(GerPos),
    #unique_effect{invinciblep=Invinciblep} = UniqueBuffer,
    %% 无敌的不受负面状态影响
    case Invinciblep of
        true ->
            invinciblep;
        _ ->
	        #gerAttr{gerHpMax=GerHpMax}=GerAttr,
            HpInc = trunc(GerHpMax*SubAttr#add_attr.gerHpMaxAddtion/10000),	
            GerHp2= GerHp - HpInc,
            GerAttr2 = add_talent_to_gerAttr(GerAttr,SubAttr),
	        %% 减血别减死了 
	        GerHp3 = 
                case GerHp2 < 1 of
                    true ->
                        1;
                    _ ->
                        GerHp2
                end,
            {Ger#ger{gerAttr=GerAttr2,gerHp=GerHp3,gerSp=max(GerSp-Sp,0)},-HpInc} 
    end;
add_talent_skill_attr(Ger,AddAttr,Sp)->
    add_talent_skill_attr(Ger,ger_attr:transOldAddAttr2NewAddAttr(AddAttr),Sp).

%% 计算天赋技能导致的怒气增量
calc_talent_inc_sp(113, Lv) ->
    calc_talent_inc_sp(103, Lv); 

calc_talent_inc_sp(103, Lv) ->
    SpInc = data_talent_skill:get(sp_inc),
    proplists:get_value(Lv,SpInc,0);

calc_talent_inc_sp(_,_) ->
    0.


before_any_action_hook(#unique_trigger{t20=T21},GerPos,AccInfo) ->
	do_unique_trigger_list(T21, GerPos,[],AccInfo).
	
%% 后续根据策划的具体需求在进行调整
%% 放绝技前的附加效果
before_unique_hook(#unique_trigger{t1=T1,t3=T3},GerPos,#attack_info{type=Type} = AccInfo) ->
   IDList =  
        case Type of
            normal ->
                [];
            unique ->
                T1;
            _ ->
                T3
        end,
    do_unique_trigger_list(IDList,GerPos,[],AccInfo).

before_attack_hook(#unique_trigger{t21=T1},GerPos,AccInfo) ->
    do_unique_trigger_list2(T1,GerPos,[],AccInfo).

%% 绝技、普通追击未命中的附加效果
miss_unique_hook(#unique_trigger{t2=T2,t4=T4},GerPos,TarGerPos,TarPosListT,#attack_info{type=Type, count=Count} = AccInfo) ->
    IDList = 
        %% 是追加攻击
        case Count =/= 0 of
            true ->
                T4#ut.miss ++ T2#ut.always;
            _ ->
                case Type of
                    normal ->
                        [];
                    unique ->
                        T2#ut.miss ++ T2#ut.always 
                end
        end,
    TarPosList = ?make_select_list(TarGerPos,TarPosListT),
    do_unique_trigger_list(IDList,GerPos,TarPosList,AccInfo).

%% 绝技、普通追击命中时的附加效果
hit_unique_hook(#unique_trigger{t2=T2,t4=T4},GerPos,DefPos,TarPosListT,#attack_info{type=Type, count=Count}=AccInfo) ->
    IDList = 
        %% 追加攻击
        case Count =/= 0 of
            true ->
				if Type == unique_noSp_2 orelse  Type == unique_noSp_4 -> T2#ut.hit ++ T2#ut.always;
				   true ->    T4#ut.hit ++ T4#ut.always
				end;
            _ ->
                case Type of
                    normal ->
                        [];
                    unique ->
                        T2#ut.hit ++ T2#ut.always
                end 
        end,
    TarPosList = ?make_select_list(DefPos,TarPosListT),
    do_unique_trigger_list(IDList,GerPos,TarPosList,AccInfo).

unique_hit_hook(#unique_trigger{t16=T16,t17=T17},GerPos,TarPosList,#attack_info{type=Type, count=Count}=AccInfo) ->
	IDList = if Count ==0 -> case Type of  unique -> T16;unique_noSp->T16; unique_noSp_4 ->T16;_ -> [] end;
				true -> case Type of unique_noSp -> T16; unique_noSp_2 -> T16;unique_noSp_4->T16;  _ -> T17 end
			 end,
	do_unique_trigger_list(IDList,GerPos,TarPosList,AccInfo).

last_action_hook(#unique_trigger{t19=T19}, GerPos,TarPosList,#attack_info{type=Type,append=Append}=AccInfo) ->
	IDList = if Type == unique_critic -> [];
				Append /= [] -> [];
				Type == normal_dot -> T19;
				true -> []
			 end,
	do_unique_trigger_list(IDList,GerPos,TarPosList,AccInfo).

%% 绝技暴击
unique_crit_hook(#unique_trigger{t5=T5},GerPos,TarGerPos,AccInfo=#attack_info{append=Append}) ->
	case Append of [] ->  do_unique_trigger_list(T5,GerPos,[TarGerPos],AccInfo);
		_ -> AccInfo
	end.

%% 被击中后
after_unique_hit_hook(#unique_trigger{t6=T6},DefPos,AtkPos,AccInfo) ->
    %%这个需要反弹普通攻击,特殊处理下
    case T6 of
        [] ->
            AccInfo;
        _ ->
            do_unique_trigger_list2(T6,DefPos,[AtkPos],AccInfo) 
    end.

maimed_hurt_hook(#unique_trigger{t25=T25},AtkPos,DefPos,AccInfo) ->
    do_unique_trigger_list2(T25, AtkPos, [DefPos],AccInfo).

%% 造成伤害后
after_damage_hook(#unique_trigger{t7=T7},AtkPos,DefPos,TarPosListT,AccInfo) ->
    TarPosList = ?make_select_list(DefPos,TarPosListT),
    do_unique_trigger_list(T7,AtkPos,TarPosList,AccInfo).

any_hurt_hook(#unique_trigger{t22=T22},AtkPos,DefPos,_TarPosListT,AccInfo) ->
	%TarPosList = ?make_select_list(DefPos,TarPosListT),
	NewAccInfo = do_unique_trigger_list2(T22, AtkPos,[DefPos],AccInfo),
	#ger{gerHp=TarGerHp} = ?get_ger(DefPos),
	if TarGerHp > 0 -> 
		   #unique_trigger{t23=T23} = ?get_unique_hook(DefPos),
		   do_unique_trigger_list2(T23, DefPos,[AtkPos],NewAccInfo);
	   true -> NewAccInfo
	end.
	%do_unique_trigger_list2(T23, AtkPos,[DefPos],NewAccInfo).
	%lists:foldl(fun(TarPos,AccInfoAcc) ->
	%					#unique_trigger{t23=T23} = ?get_unique_hook(TarPos),
	%					do_unique_trigger_list2(T23, TarPos,[AtkPos],AccInfoAcc)
	%					end, NewAccInfo, TarPosList).

after_unheadledIs_hook(#unique_trigger{t18=T18},SrcPos,TarPosList,Acc) ->
	do_unique_trigger_list(T18,SrcPos,TarPosList,Acc).
	

after_unique_attack_hook(#unique_trigger{t9=T9},AtkPos,TarPosList,AccInfo) ->
    do_unique_trigger_list(T9,AtkPos,TarPosList,AccInfo).

after_strike_back(#unique_trigger{t12=T12},AtkPos,TarPosList,AccInfo) ->
	do_unique_trigger_list2(T12,AtkPos,TarPosList,AccInfo).

after_normal_attack_hook(#unique_trigger{t13=T13},SkillType,AtkPos,TarPosList,AccInfo) ->
	if T13 == [] -> AccInfo;
	   SkillType == normal orelse SkillType == normal2 orelse SkillType == normal_2  -> do_unique_trigger_list2(T13,AtkPos,TarPosList,AccInfo);
	   true -> AccInfo
	end.

after_unique_attack(#unique_trigger{t24=T13},SkillType,AtkPos,TarPosList,AccInfo) ->
	if T13 == [] -> AccInfo;
	   SkillType == unique orelse SkillType == unique2 -> do_unique_trigger_list2(T13,AtkPos,TarPosList,AccInfo);
	   true -> AccInfo
	end.

after_normal_dot_hook(#unique_trigger{t14=T14},SrcGerPos,TarPosList,NewAcc) ->
	if T14 == [] -> NewAcc;
	   true -> 
		   #attack_info{append=Append} = AtkAcc2 = do_unique_trigger_list2(T14,SrcGerPos,TarPosList,NewAcc),
			AtkAcc2#attack_info{append=Append}
end.

after_crit_hook(#unique_trigger{t15=T15},SrcGerPos,TarPosList,#attack_info{type=Type}=NewAcc) ->
	if Type == unique_critic ->do_unique_trigger_list2(T15,SrcGerPos,TarPosList,NewAcc);
	   true -> NewAcc 
	end.

do_unique_trigger_list(IDList,GerPos,TargetList,#attack_info{type=Type,count=Count}=AccInfo) ->
    case ((Type =:= normal orelse Type == normal_2) andalso Count =:= 0) orelse IDList =:= [] of
        true ->
            AccInfo;
        _ ->
            do_unique_trigger_list2(IDList,GerPos,TargetList,AccInfo)
    end.

do_unique_trigger_list2([],_,_,Acc) ->
    Acc;
%% UnhandledsAcc 是在这儿触发,但需要在之后处理的效果
%% ActionList 是在这个就播放就播放的效果
%% AlreadyAcc 记录是否已经触发过了,防止群攻时,一个效果多次触发
do_unique_trigger_list2([H|T],GerPos,TargetList,#attack_info{uaction=ActionList,unhandleds=#uh{c1=C1Acc,c2=C2Acc}=UhAcc,trigger=AlreadyAcc,count=ACount,
                                                                nstrike_back=NStrikeBack,ustrike_back=UStrikeBack,damage=Damage,multirecord=MRAcc}=Acc) ->
    case data_unique_effect:get(H) of
        undefined ->
            do_unique_trigger_list2(T,GerPos,TargetList,Acc); 
        #data_unique_effect{probability=Probability1,duration=Duration,apply_target=ApplyTarget,
                             apply_type=ApplyType,args=Args,multip=Multip, select_rage_p=SelectRageP}->
            %%此处为觉醒技能增加晶体系统带来的额外触发概率
            #ger{gerBase=#gerBase{gerTypeID=GerTypeID}}=SrcGer = ?get_ger(GerPos),
			CrystalIncValue = role_crystal:calculate_crystal_awake_add(SrcGer#ger.gerBase#gerBase.gerCrystalInfo,5),
			Probability = case lists:member(ApplyType,[25,21,26,22,24,23]) of
							  true->  Probability1+CrystalIncValue/100;
							  false->  case ApplyType of 60 -> lists:nth(ACount+1, lists:nth(2,Args));
										   _ -> Probability1
									   end
						  end,
			#unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(GerPos),
			Skill_ra_up = UniqueBuffer#unique_effect.skill_ra_up,
			DiamondAdd = lists:foldl(fun({DiamondRaUpIDList,DiamondUpRa},DiamondAddAcc) ->
											 case lists:member(H, DiamondRaUpIDList) of
												 true -> DiamondAddAcc + DiamondUpRa;
												 _ -> DiamondAddAcc
											 end end, 0, Skill_ra_up),
			RandowmX = random:uniform() * 100,
			IsWin = case enemy_alive(GerPos) of [] -> true; _-> false end,
            case Probability + DiamondAdd =< RandowmX orelse IsWin of
                true ->
                    do_unique_trigger_list2(T,GerPos,TargetList,Acc); 
                _ ->
                    %% 已经触发过这个效果了,则不在触发
                    case lists:member(H,AlreadyAcc) of
                        true ->
                            do_unique_trigger_list2(T,GerPos,TargetList,Acc); 
						_ ->
							%%TODO 现在有爆怒技能,选择时会变换选择的阵营,然后在新技能中目前只要求修改狐狸,所以这儿进行了特殊处理
							%%然后之前的新技能等暂时不做更改,如果都需要进行处理,则修改这里,而且都需要重新进行测试
							#unique_effect{in_rage=InRage,dead_mark=DeadMark} = UniqueBuffer,
							SelectPos = 
                                case SelectRageP of
                                    true ->
                                        case InRage of
                                            true ->
                                                %% 对向对应位置有精灵,就用这个精灵,没有则选择对面第一个活着的
                                                Enemys = enemy_alive(GerPos),
                                                case lists:member(-GerPos, Enemys) of
                                                    true ->
                                                        -GerPos;
                                                    _ ->
                                                        erlang:hd(Enemys)
                                                end;
                                            _ ->
                                                GerPos
                                        end; 
                                    _ ->
                                        GerPos
                                end,
                            {ApplyListT,_} = get_applylist_attr(unique,sub_attr,SelectPos,ApplyTarget,TargetList,DeadMark,UhAcc,{InRage,GerPos,ACount}),
                            %% 暴怒,如果改变了选择对象阵营,则要过滤掉自己,如果最后是空列表,则按照正常的来选
                            ApplyList =
                                case InRage of
                                    true ->
                                        case lists:delete(GerPos, ApplyListT) of
                                            [] ->
                                                {ApplyListT2,_} = get_applylist_attr(unique,sub_attr,GerPos,ApplyTarget,TargetList,DeadMark,UhAcc,{InRage,GerPos,ACount}),
                                                ApplyListT2;
                                            AList ->
                                                AList
                                        end;
                                    _ ->
                                        ApplyListT
                                end,
                            {NewAlreadyAcc, NewMRAcc} = 
                                case Multip of
                                    true ->
                                        {AlreadyAcc,[H|MRAcc]};
                                    _ ->
                                        {[H|AlreadyAcc],MRAcc}
                                end,
                            NewAcc = 
                                %%multirecord这个是用来解决黑龙脆弱的问题,其他的目前可以不用记
                                case ApplyType of 
                                    6 ->
                                        %% 对于附加效果是眩晕的需要特殊处理,为在攻击目标中随机选者一个眩晕
                                        Acc#attack_info{unhandleds=UhAcc#uh{c1=[{H,TargetList,GerPos}|C1Acc]},trigger=NewAlreadyAcc,multirecord=NewMRAcc};
                                    18 ->
                                        %% 伤害链接的流程和眩晕是一样的
                                        Acc#attack_info{unhandleds=UhAcc#uh{c1=[{H,TargetList,GerPos}|C1Acc]},trigger=NewAlreadyAcc,multirecord=NewMRAcc};
                                    57 ->
                                        %% 伤害链接的流程和眩晕是一样的
                                        Acc#attack_info{unhandleds=UhAcc#uh{c1=[{H,TargetList,GerPos}|C1Acc]},trigger=NewAlreadyAcc,multirecord=NewMRAcc};
                                    85 ->
                                        %% 伤害链接的流程和眩晕是一样的
                                        Acc#attack_info{unhandleds=UhAcc#uh{c1=[{H,TargetList,GerPos}|C1Acc]},trigger=NewAlreadyAcc,multirecord=NewMRAcc};
         
                                    4 -> 
                                        %% 追加普通+暴击攻击
                                        update_append_list(Acc, hit_critic, ApplyList, NewAlreadyAcc, Args);
                                    7 ->
                                        %% 追加普通攻击
                                        #unique_buffer{use=UseEffect7} =UniqueBufferApply7= ?get_unique_buffer(GerPos),
										[[DamageDoubleArg,_]] = Args,
										UseEffect7_2 = UseEffect7#unique_effect{damage_double=DamageDoubleArg},
                                        ?set_unique_buffer(GerPos,UniqueBufferApply7#unique_buffer{use=UseEffect7_2}),
                                        update_append_list(Acc, normal, ApplyList, NewAlreadyAcc, Args);
                                    8 ->
                                        %% 追加绝技攻击
                                        update_append_list(Acc, unique, ApplyList, NewAlreadyAcc, Args);
                                    9 ->
                                        %% 怒气特殊处理,因为要在受击攻击后才播放(加给自己的)
                                        Acc#attack_info{unhandleds=UhAcc#uh{c2=[{H,SelectPos,GerPos}|C2Acc]},trigger=NewAlreadyAcc,multirecord=NewMRAcc};
                                    12 ->
                                        add_dot_buffer(ApplyList,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},Duration,Args,SelectPos);
                                    37 ->
                                    	%%为格斗系觉醒技能伤害值加上晶体系统添加的增益
                                    	SrcGer = ?get_ger(GerPos),
                        				CrystalIncValue = role_crystal:calculate_crystal_awake_add(SrcGer#ger.gerBase#gerBase.gerCrystalInfo,5),
                        				[TypeArg37,H_Arg_37|T_Arg_37] = Args,
                        				NewValue = trunc(H_Arg_37+CrystalIncValue*10),
                                        add_dot_buffer(ApplyList,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},Duration,[TypeArg37,NewValue]++T_Arg_37,SelectPos);
                                    14 ->
										[Ratio,ActionType]=Args,
                                        Acc#attack_info{nstrike_back=[{SelectPos,Damage,Ratio,ActionType,GerTypeID}|NStrikeBack]};
                                    15 ->
										[Ratio,ActionType]=Args,
                                        Acc#attack_info{ustrike_back=[{SelectPos,Damage,Ratio,ActionType,GerTypeID}|UStrikeBack]};
                                    21 ->
                                        %%爆炸效果在所有攻击处理完之后再放
                                        Acc#attack_info{unhandleds=UhAcc#uh{c1=[{H,TargetList,GerPos}|C1Acc]},trigger=NewAlreadyAcc,multirecord=NewMRAcc};
                                    24 ->
                                        %% 急速
                                        update_append_list(Acc, speed, ApplyList, NewAlreadyAcc, Args);
                                    %% 迷惑
                                    27 ->
                                        %% 这个需要过滤掉boss
                                        ApplyList2 = 
                                            lists:filter(fun(AE) ->
                                                            #ger{gerBase=GerBase} = ?get_ger(AE),
                                                            not is_Boss_fight_type(GerBase#gerBase.gerTypeID)
                                                        end, ApplyList),
                                        NewAtkInfo = Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},
                                        case ApplyList2 of
                                            [] ->
                                                NewAtkInfo;
                                            _ ->
												[_, ActionType,_] = Args,
												if ActionType == 3 ->
														Acc#attack_info{unhandleds=UhAcc#uh{c1=[{H,ApplyList2,SelectPos}|C1Acc]}
																	   ,trigger=NewAlreadyAcc,multirecord=NewMRAcc};
												   true ->
													   do_confuse(SelectPos, ApplyList2, H, Duration, Args, NewAtkInfo)
												end
                                        end;
									 47 ->
                                        %% 这个需要过滤掉boss
                                        ApplyList2 = 
                                            lists:filter(fun(AE) ->
                                                            #ger{gerBase=GerBase} = ?get_ger(AE),
                                                            not is_Boss_fight_type(GerBase#gerBase.gerTypeID)
                                                        end, ApplyList),
                                        NewAtkInfo = Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},
                                        case ApplyList2 of
                                            [] ->
                                                NewAtkInfo;
                                            _ ->
                                                do_confuse_multi(SelectPos, ApplyList2, H, Duration, Args, NewAtkInfo) 
                                        end;
                                    30 ->
                                        camp_clear_buff(SelectPos, Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc});
                                    31 ->
                                        %% 普通追加+dot
                                        update_append_list(Acc, normal_dot, ApplyList, NewAlreadyAcc, Args);
                                    32 ->
                                        %% 必杀暴击
                                        update_append_list(Acc, unique_critic, ApplyList, NewAlreadyAcc, Args);
                                    33 ->
                                        %% 沉睡暴击
                                        trigger_morph_sleep(SelectPos, Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc}, H, Duration, Args);
                                    35 ->
                                        do_morph_sing(SelectPos,ApplyList,H,Duration,Args,0,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc});
                                    36 ->
                                        do_morph_sing(SelectPos,ApplyList,H,Duration,Args,1,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc});
									38 -> update_append_list(Acc, unique_noSp, ApplyList,NewAlreadyAcc,Args);
									48 -> update_append_list(Acc, unique_noSp_2, ApplyList,NewAlreadyAcc,Args);
%% 									40 ->
%% 										update_append_list(Acc, holy_kill, ApplyList,NewAlreadyAcc,Args);
									41 ->
                                        update_append_list(Acc, normal_crazy, ApplyList, NewAlreadyAcc, Args);
									45 ->	
                                        %% 追加普通攻击
                                        update_append_list(Acc, normal_2, ApplyList, NewAlreadyAcc, Args);
									43 -> do_unique_flow(GerPos,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},Args);
									49 -> update_append_list(Acc, normal_3, ApplyList, NewAlreadyAcc, Args);
									50 ->  %% 追加绝技攻击 必暴击
                                        update_append_list(Acc, unique_critic_2, ApplyList, NewAlreadyAcc, Args);
									51 ->
                                        %% 追加普通攻击
                                        update_append_list(Acc, normal_4, ApplyList, NewAlreadyAcc, Args);
%% 									53 ->
%%                                         %% 追加绝技攻击
%%                                         update_append_list(Acc, unique3, ApplyList, NewAlreadyAcc, Args);
									54 ->  %% 追加绝技攻击 必暴击
                                        update_append_list(Acc, unique_critic_3, ApplyList, NewAlreadyAcc, Args);
									60 -> %% 追加绝技攻击
                                        update_append_list(Acc, unique_noSp_3, ApplyList, NewAlreadyAcc, Args);
									63 ->
										freeze_ground(GerPos,ApplyList,Duration,Args,Acc);
									64 ->
										cp_fighter(GerPos,Acc);
									58 -> %% 追加绝技攻击
                                        update_append_list(Acc, unique_noSp_4, ApplyList, NewAlreadyAcc, Args);
                                    %%77 ->
                                    %%    dec_sp(ApplyList,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},Args,SelectPos);
                                    78 ->
                                        damage_double(GerPos,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc},Args);
                                    79 ->
                                        update_append_list(Acc, normal_5, ApplyList, NewAlreadyAcc, Args);
                                    81 ->
                                        do_morph_sing_b(SelectPos,ApplyList,H,Duration,Args,0,Acc#attack_info{trigger=NewAlreadyAcc,multirecord=NewMRAcc});
                                    84 -> 
                                        unReel_ger(Acc,GerPos);
                                    _ ->
                                        {UnhandledsAcc3,ActionAcc2} = 
                                            lists:foldl(fun(Pos,{#uh{c1=C1Acc2}=UhAcc2,ActionAcc}) -> 
                                                            case apply_unique_effect(SelectPos,H,ApplyType,Pos,Duration,Args) of
                                                                {_,_,_} = Info ->
                                                                    {UhAcc2#uh{c1=[Info|C1Acc2]},ActionAcc};
                                                                Action ->
                                                                    {UhAcc2, Action ++ ActionAcc}
                                                            end
                                                        end,{UhAcc,[]},ApplyList),    
										%%                                        %% 有效眩晕需要加上自己的释放动作
										ActionList2 = 
											if ApplyType =:= 16 andalso ActionAcc2 =/= [] ->
												   %% 暴怒后的释放动作不影响
												   case hd(Args) of
													   1 -> ActionAcc2 ++ [?action(118,GerPos,[],0,0,?STATE_DEFAULT)|ActionList];
													   2 -> ActionAcc2 ++ [?action(198,GerPos,[],0,0,?STATE_DEFAULT)|ActionList]
												   end;
											   ApplyType == 44 andalso ActionAcc2 =/= [] ->
												   ActionAcc2 ++ [?action(187,GerPos,[],0,0,?STATE_DEFAULT)|ActionList];
											   ApplyType == 76 andalso ActionAcc2 =/= [] ->
												   ActionAcc2 ++ [?action(255,GerPos,[],0,0,?STATE_DEFAULT)|ActionList];
											   true ->
												   ActionAcc2 ++ ActionList
											end,

                                        Acc#attack_info{uaction=ActionList2,unhandleds=UnhandledsAcc3,trigger=NewAlreadyAcc,multirecord=NewMRAcc}
                                end,
                            do_unique_trigger_list2(T,GerPos,TargetList,NewAcc)
                    end
            end 
    end.

do_holy_damage(SrcGerPos,Type)->
	#unique_trigger{t10=Tri1,t11=Tri2} = ?get_unique_hook(SrcGerPos),
	case Type of 1 -> do_holy_damage(Tri1,SrcGerPos,[]);
		2 -> do_holy_damage(Tri2,SrcGerPos,[]);
		_ ->[]
	end.
do_holy_damage([],_,Action) -> Action;
do_holy_damage([H|T],SrcGerPos,Action) ->
	case data_unique_effect:get(H) of
		undefined ->
			do_holy_damage(T,SrcGerPos,Action);
		#data_unique_effect{apply_target=ApplyTarget,args=Args}->
			Ra = hd(Args),
			{ApplyListT,_} = get_applylist_attr(unique,sub_attr,SrcGerPos,ApplyTarget,[],0,0,{0,0,0}),
			{ActionX,ActionA} = 
				lists:foldl(fun(E,{ActionXAcc,ActionAcc}) ->
									#unique_buffer{use=UseEffect} = ?get_unique_buffer(E),
									#unique_effect{invinciblep=Invinciblep,spRa=SpRa} = UseEffect,
									case Invinciblep of
										true ->
											{ActionXAcc,[?action(?ACTION_NORMAL_HURT,E,[],0,0,?STATE_DEFAULT)|ActionAcc]};
										_ ->
											#ger{gerHp=TarHp,gerProHp=TarPropHp,gerAttr=#gerAttr{gerHpMax=TarGerHpMax}=TarAttr,gerSp=TarSp}=TarGer=?get_ger(E),
											HarmValueT1 = trunc(TarGerHpMax * Ra / 100),
											{Inv0,HarmValue} = check_d_minHp(E,HarmValueT1),
											damage_stastic(SrcGerPos,HarmValue),
											InvAction = if Inv0 -> set_d_inv(); true -> [] end,
											{ProHpDecf,HpDecf} = decf_ger_hp(TarPropHp,HarmValue),
											TarHp2 = add_hp(TarHp,TarAttr#gerAttr.gerHpMax,HpDecf),
											TarProHp2 = TarPropHp + ProHpDecf,
											TarAddSp = trunc(real_add_sp(TarSp,?HURT_ADD_SP,(TarGer#ger.gerAttr)#gerAttr.gerSpMax) * SpRa / 100),
											TarSp2 = TarSp + TarAddSp,
											TarGer2 = TarGer#ger{gerHp=TarHp2,gerProHp=TarProHp2,gerSp=TarSp2},
											?set_ger(TarGer2),
											case TarHp2 =< 0 of
												true ->
													{IsReborn,RebornAction,_KillHooks} = do_reborn(SrcGerPos,E,TarGer2,{0,[],[]}),
													if IsReborn == false ->
														   {InvAction++ActionXAcc, [?pro_action(?ACTION_NORMAL_HURT,E,[],HpDecf,ProHpDecf,0,?STATE_DEAD)|ActionAcc]};
													   true ->
														   {InvAction++RebornAction ++ ActionXAcc,[?pro_action(?ACTION_NORMAL_HURT,E,[],HpDecf,ProHpDecf,0,?STATE_DEFAULT)|ActionAcc]}
													end;
												_ -> {InvAction++ActionXAcc,[?pro_action(?ACTION_NORMAL_HURT,E,[],HpDecf,ProHpDecf,TarAddSp,?STATE_DEFAULT)|ActionAcc]}
											end
									end
							end,{[],[?action(177,P,[],0,0,?STATE_DEFAULT)||P<-ApplyListT]++[?action(193,SrcGerPos,[],0,0,?STATE_DEFAULT),?action(176,SrcGerPos,[],0,0,?STATE_DEFAULT)]},ApplyListT),
			do_holy_damage(T,SrcGerPos,ActionX++ActionA++Action)
	end.

%|[?action(193,P,[],0,0,?STATE_DEFAULT)||P<-ApplyListT]


%% 训练师技能周期都比较短，不会重复施法。训练师effecID为0
apply_trSkill_effect(Actions) ->
	[A#p_action{actionID=ID+1}||#p_action{actionID=ID}=A<-Actions] ++Actions.
apply_trSkill_effect(Pos,Duration,Value,Buff,ApplyType,SpecialID) ->
	UniqueBuffer = #unique_buffer{use=UseEffect,effects=Effects} = ?get_unique_buffer(Pos),
	{NewUseEffect,Action} = add_trskill_unique_effect(Pos,UseEffect,ApplyType,Value,Buff,Duration),
	?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=NewUseEffect,
													  effects=[#effect_record{effectId=0,duration=Duration-1
																			  ,trSkill= {ApplyType,Value,SpecialID}}
																				 |Effects]}
	),
	Action.
                
apply_unique_effect(SrcPos,EffectID,ApplyType,Pos,Duration,Args) ->
    case lists:member(ApplyType,[1,2,3,10,11,12,13,16,23,37,39,56,83]) of
        true ->
            UniqueBuffer = #unique_buffer{use=UseEffect,effects=Effects} = ?get_unique_buffer(Pos),
            case lists:keytake(EffectID,#effect_record.effectId,Effects) of
                false ->
                    %% 添加effects,并记录
                    {NewUseEffect,Action} = add_unique_effect(SrcPos,Pos,UseEffect,ApplyType,Args,Duration,EffectID),
                    ?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=NewUseEffect,effects=[#effect_record{effectId=EffectID,duration=Duration}|Effects]}),
					Action;
				{value,#effect_record{effectId=LastEffectID,duration=LastDuration},Effects2} ->
					%% 刷新effects的时效
					if LastEffectID == 1008 orelse LastEffectID == 1015 -> Effect =  #effect_record{effectId=EffectID,duration=Duration+LastDuration};
					   true -> Effect = #effect_record{effectId=EffectID,duration=Duration} end,
					?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{effects=[Effect|Effects2]}),
                    case ApplyType of
                        11 ->
                            %% 命中刷新时也要播放动画
                            [?action(112,Pos,[],0,0,?STATE_TALENT)];
                        16 ->
                            %% 沉睡始终要刷新
                            %% 此处判断是否为挑战世界Boss，如果是世界BOSS，将免疫昏睡
                            TarGer = ?get_ger(Pos),
                            %GerTypeID = TarGer#ger.gerBase#gerBase.gerTypeID,
                            case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
                                true->
                                    [?action(120,Pos,[],0,0,?STATE_TALENT)];
                                false->
                                    case update_target_maimed(SrcPos,Pos,6,Duration) of
                                        invinciblep -> [];
                                        _ ->[?action(116,Pos,[],0,0,?STATE_TALENT)]
                                    end
                            end;
                        _ ->
                            []
                    end
            end;
        _ ->
			case ApplyType of
				22->
					Ger = ?get_ger(Pos),
					AddHp = erlang:trunc(?a(Ger,gerHpMax) * erlang:hd(Args)/100),
					NewHp = add_hp(Ger#ger.gerHp,?a(Ger,gerHpMax),AddHp),
					NewGer = Ger#ger{gerHp=NewHp},
					?set_ger(NewGer),
					[?action(101,Pos,[],AddHp,0,?STATE_DEFAULT)];
				42 ->
					UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
					#unique_effect{damage_double=DamageDouble} = UseEffect,
					?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{damage_double=DamageDouble+1}}),
					[];
				44 ->
					case update_target_maimed(SrcPos,Pos,174,Duration,EffectID) of
						invinciblep ->
							[];
						_ ->
							UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
							#unique_effect{damage_add={DamageAdd,AcList}} = UseEffect,
							AcList2 = [SrcPos|lists:delete(SrcPos, AcList)],
							?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{damage_add={DamageAdd + erlang:hd(Args),AcList2}}}),
							[?action(173,Pos,[],0,0,?STATE_TALENT),?action(172,Pos,[],0,0,?STATE_TALENT)]
					end;
				76 ->
					case update_target_maimed(SrcPos,Pos,257, Duration,EffectID) of
						invinciblep -> [];
						_ ->
							UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
							?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{parry=true}}),
							[?action(257,Pos,[],0,0,?STATE_TALENT),?action(256,Pos,[],0,0,?STATE_TALENT)]
					end;
				29 ->
					case update_target_maimed(SrcPos,Pos,143,Duration,EffectID) of
						invinciblep -> [];
						_ ->	
							UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
							#unique_effect{damage_sub={DamageSub,SList}, atk_dmg_plus={ADP,ADList,RList,ZList}} = UseEffect,
							Action = [?action(143,Pos,[],0,0,?STATE_TALENT)],
							Value = erlang:hd(Args),
							?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{damage_sub={DamageSub + Value,SList}
																										 , atk_dmg_plus={ADP + Value,ADList,RList,ZList}}}),
							Action
					end;
                82 ->
                    case update_target_maimed(SrcPos,Pos,143,Duration,EffectID) of
                        invinciblep -> [];
                        _ ->    
                            UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
                            #unique_effect{atk_dmg_plus={ADP,ADList,RList,ZList}} = UseEffect,
                            Action = [?action(143,Pos,[],0,0,?STATE_TALENT)],
                            Value = erlang:hd(Args),
                            ?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{atk_dmg_plus={ADP + Value,ADList,RList,ZList}}}),
                            Action
                    end;
				61 ->
					UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
					?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{damage_double=erlang:hd(Args)}}),
					[];					
				62 ->
					UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
					#unique_effect{atk_dmg_plus={DamagePlus,ADList,RList,ZList},ningdong={NingDong,_}} = UseEffect,
					BuffV=hd(Args),
                    {NingDong2,DamagePlus2} = if NingDong < 3 -> {NingDong+1,DamagePlus + BuffV};%erlang:min(NingDong+1, 3),
                                                    true -> {NingDong, DamagePlus}
                                              end,
					?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect#unique_effect{ningdong={NingDong2,BuffV},atk_dmg_plus={DamagePlus2,ADList,RList,ZList}}}),
					[?action(219,Pos,[],NingDong2,0,?STATE_TALENT)];
				_ ->
					{EffectID,Pos,SrcPos}
			end
	end.
%% 训练师绝技附加属性
add_trskill_unique_effect(Pos,UseEffect,ApplyType,Value,_Buff,_Duration) ->
	case ApplyType of
		5 ->
			#unique_effect{damage_add={DamageAdd,AcList}} = UseEffect,
			%Action = [?action(?ACTION_TR_RIP3,Pos,[],0,0,?STATE_DEFAULT),?action(?ACTION_TR_RIP2,Pos,[],0,0,?STATE_DEFAULT)],
			Action = [?action(?ACTION_TR_RIP2,Pos,[],0,0,?STATE_DEFAULT)],
			{UseEffect#unique_effect{damage_add={DamageAdd + Value,AcList}},Action};
        7 ->
			
            #unique_effect{attack_add=DamagePlus} = UseEffect,
			%Action = [?action(?ACTION_TR_THIRTY3,Pos,[],0,0,?STATE_DEFAULT),?action(?ACTION_TR_THIRTY2,Pos,[],0,0,?STATE_DEFAULT)],
			Action = [?action(?ACTION_TR_THIRTY2,Pos,[],0,0,?STATE_DEFAULT)],
            {UseEffect#unique_effect{attack_add=DamagePlus + Value},Action};
		8 ->
            #unique_effect{damage_dec=DamageDec} = UseEffect,
            %% 减伤的action在这儿放
            %Action = [?action(?ACTION_TR_STONE3,Pos,[],0,0,?STATE_DEFAULT),?action(?ACTION_TR_STONE2,Pos,[],0,0,?STATE_DEFAULT)],
			Action = [?action(?ACTION_TR_STONE2,Pos,[],0,0,?STATE_DEFAULT)],
            {UseEffect#unique_effect{damage_dec=DamageDec + Value},Action};
        9 ->
			#unique_effect{hit_critic={_,HList}} = UseEffect,
			%Action = [?action(?ACTION_TR_KNOW3,Pos,[],0,0,?STATE_DEFAULT),?action(?ACTION_TR_KNOW2,Pos,[],0,0,?STATE_DEFAULT)],
			Action = [?action(?ACTION_TR_KNOW2,Pos,[],0,0,?STATE_DEFAULT)],
            {UseEffect#unique_effect{hit_critic={true,HList}},Action}
	end.
%% 加绝技附加效果
add_unique_effect(SrcPos,Pos,UseEffect,ApplyType,Args,Duration,EffectID) ->
    case ApplyType of
        1 ->
            #unique_effect{damage_sub={DamageSub,SList}} = UseEffect,
            [DamageSubArg,ActionType] = Args,
            ActionTypeActionID = case ActionType of 1 -> 106;2 -> 268 end,
            %% 减伤的action在这儿放
            Action = [?action(ActionTypeActionID,Pos,[],0,0,?STATE_TALENT)],
            {UseEffect#unique_effect{damage_sub={DamageSub + DamageSubArg,SList}},Action};
        83 ->
            #unique_effect{damage_sub={DamageSub,SList},spRa=NSpRa} = UseEffect,
            [DamageSubArg,SpRa] = Args,
            %% 减伤的action在这儿放
            Action = [?action(106,Pos,[],0,0,?STATE_TALENT)],
            {UseEffect#unique_effect{damage_sub={DamageSub + DamageSubArg + erlang:hd(Args),SList},spRa = NSpRa-SpRa},Action};
        2 ->
            #unique_effect{critic_plus=CriticPlus} = UseEffect,
            {UseEffect#unique_effect{critic_plus=CriticPlus + erlang:hd(Args)},[]};
        3 ->
            #unique_effect{damage_plus={DamagePlus,AList,BList}} = UseEffect,
            {UseEffect#unique_effect{damage_plus={DamagePlus + erlang:hd(Args),AList,BList}},[]};
        10 ->
			case hd(Args) of
				1 ->
					%% 无敌的特效
					HAction = do_holy_damage(SrcPos,1),
					Action = HAction++[?action(111,Pos,[],0,0,?STATE_TALENT)],
					#unique_effect{inv_way={InvWay,IList}} = UseEffect,
					InvWay2 = [skill|lists:delete(skill, InvWay)],
					{UseEffect#unique_effect{invinciblep=true,inv_way={InvWay2,IList}},Action}; 
				2 ->
					Action = [?action(111,Pos,[],0,0,?STATE_TALENT),?action(229,Pos,[],0,0,?STATE_TALENT),?action(228,Pos,[],0,0,?STATE_TALENT)],
					#unique_effect{inv_way={InvWay,IList}} = UseEffect,
					InvWay2 = [diamond|lists:delete(diamond, InvWay)],
					{UseEffect#unique_effect{invinciblep=true,inv_way={InvWay2,IList}},Action}
			end; 
        11 ->
            #unique_effect{hit_plus=HitPlus} = UseEffect,
            %% 加命中的动画
            Action = [?action(112,Pos,[],0,0,?STATE_TALENT)],
            {UseEffect#unique_effect{hit_plus=HitPlus + erlang:hd(Args)},Action};
        13 ->
            #unique_effect{cure_sub=CurSub} = UseEffect,
            {UseEffect#unique_effect{cure_sub=CurSub + erlang:hd(Args)},[]};
        16 ->
        	TarGer = ?get_ger(Pos),
            Action = 
             	case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
                    true->
                        [?action(120,Pos,[],0,0,?STATE_TALENT)];
                    false->
                        %%为攻击目标加上需要其解除的昏睡目标
                        case update_target_maimed(SrcPos,Pos,?ACTION_UNSLEEP,Duration,EffectID) of
                            invinciblep -> [];
                            _ ->[?action(116,Pos,[],0,0,?STATE_TALENT)]
                        end
                end,
            {UseEffect,Action};
        %%加个石化
        23 ->
            #unique_effect{damage_sub={DamageSub,SList}} = UseEffect,
            %% 减伤的action在这儿放
            Action = [?action(81,Pos,[],0,0,?STATE_TALENT)],
            {UseEffect#unique_effect{damage_sub={DamageSub + erlang:hd(Args),SList}},Action};
		39 ->
			{UseEffect#unique_effect{damage_base_blood=erlang:hd(Args)},[]};
		42 ->
			#unique_effect{damage_double=DamageDouble} = UseEffect,
			{UseEffect#unique_effect{damage_double=DamageDouble+1},[]};
		52 ->
			#unique_effect{damage_add={DamageAdd,AcList}} = UseEffect,
			{UseEffect#unique_effect{damage_add={DamageAdd+erlang:hd(Args),AcList}},[?action(107,Pos,[],0,0,?STATE_TALENT)]};
		56 ->
			#unique_effect{damage_maxHp=DamageAdd} = UseEffect,
			{UseEffect#unique_effect{damage_maxHp=DamageAdd+erlang:hd(Args)},[?action(196,Pos,[],0,0,?STATE_TALENT)]};
        _ ->
            {UseEffect,[]}
    end.

%% 减绝技附加效果 
%% 训练师技能effectID为0
sub_tr_effect(Pos,UseEffect,0,{ApplyType,Value,_SpecialID})->
	case ApplyType of
		5 ->
			#unique_effect{damage_add={DamageAdd,AcList}} = UseEffect,
			Action = ?action(?ACTION_TR_RIP4,Pos,[],0,0,?STATE_DEFAULT),
			{UseEffect#unique_effect{damage_add={DamageAdd - Value,AcList}},Action};
		7 ->
			#unique_effect{attack_add=DamagePlus} = UseEffect,
            Action = ?action(?ACTION_TR_THIRTY4,Pos,[],0,0,?STATE_DEFAULT),
            {UseEffect#unique_effect{attack_add=DamagePlus - Value},Action};
		8 ->
			#unique_effect{damage_dec=DamageSub} = UseEffect,
            Action = ?action(?ACTION_TR_STONE4,Pos,[],0,0,?STATE_DEFAULT),
            {UseEffect#unique_effect{damage_dec=DamageSub - Value},Action};
		9 ->
			#unique_effect{hit_critic={_,HList}} = UseEffect,
			Action = ?action(?ACTION_TR_KNOW4,Pos,[],0,0,?STATE_DEFAULT),
			{UseEffect#unique_effect{hit_critic={false,HList}},Action}
	end.

sub_unique_effect(Pos,UseEffect,EffectID,SubType,Params) ->
    #data_unique_effect{apply_type=ApplyType,args=Args} = data_unique_effect:get(EffectID),
    case ApplyType of
        1 ->
            [DamageSubArg,ActionType] = Args,
            ActionTypeActionID = case ActionType of 1 -> ?ACTION_DAMAGE_PLUS_RMV;2 -> 269 end,
            #unique_effect{damage_sub={DamageSub,SList}} = UseEffect,
            Action = [?action(ActionTypeActionID,Pos,[],0,0,?STATE_DEFAULT)],
            {UseEffect#unique_effect{damage_sub={DamageSub - DamageSubArg,SList}},Action};
        83 ->
            #unique_effect{damage_sub={DamageSub,SList},spRa=NSpRa} = UseEffect,
            [DamageSubArg,SpRa] = Args,
            %% 减伤的action在这儿放
            Action = [?action(?ACTION_DAMAGE_PLUS_RMV,Pos,[],0,0,?STATE_TALENT)],
            {UseEffect#unique_effect{damage_sub={DamageSub - DamageSubArg,SList},spRa = NSpRa+SpRa},Action};
        2 ->
            #unique_effect{critic_plus=CriticPlus} = UseEffect,
            {UseEffect#unique_effect{critic_plus=CriticPlus - erlang:hd(Args)},[]};
        3 ->
            #unique_effect{damage_plus={DamagePlus,AList,BList}} = UseEffect,
            {UseEffect#unique_effect{damage_plus={DamagePlus - erlang:hd(Args),AList,BList}},[]};
        10 ->
			case hd(Args) of
				1 ->
					HAction = do_holy_damage(Pos,2),
					Action = HAction++[?action(?ACTION_INVINCIBLEP_RMV,Pos,[],0,0,?STATE_DEFAULT)],
					#unique_effect{inv_way={InvWay,IList}} = UseEffect,
					InvWay2 = lists:delete(skill, InvWay),
					case InvWay2 of [] -> {UseEffect#unique_effect{invinciblep=false,inv_way={[],IList}},Action};
						_ -> {UseEffect#unique_effect{inv_way={InvWay2,IList}},Action}
					end;
				2 -> 
					Action = [?action(?ACTION_INVINCIBLEP_RMV,Pos,[],0,0,?STATE_DEFAULT)],
					#unique_effect{inv_way={InvWay,IList}} = UseEffect,
					InvWay2 = lists:delete(diamond, InvWay),
					case InvWay2 of [] -> {UseEffect#unique_effect{invinciblep=false,inv_way={[],IList}},Action};
						_ -> {UseEffect#unique_effect{inv_way={InvWay2,IList}},Action}
					end
			end;
        11 ->
            #unique_effect{hit_plus=HitPlus} = UseEffect,
            {UseEffect#unique_effect{hit_plus=HitPlus - erlang:hd(Args)},[]}; 
        13 ->
            #unique_effect{cure_sub=CurSub} = UseEffect,
            {UseEffect#unique_effect{cure_sub=CurSub - erlang:hd(Args)},[]};
        27 ->
            case SubType of
                {allure, 0} ->
                    Action = [?action(?ACTION_ALLURE_RMV_ID,Pos,[],0,0,?STATE_DEFAULT)],
                    {UseEffect, Action};
				{allure, 1} ->
                    Action = [?action(245,Pos,[],0,0,?STATE_DEFAULT)],
                    {UseEffect, Action};
				{allure, 2} ->
                    Action = [?action(245,Pos,[],0,0,?STATE_DEFAULT)],
                    {UseEffect, Action};
                {rage, Inc} ->
					 #unique_effect{atk_dmg_plus={DP,ADList,RList,ZList}} = UseEffect,
					RList2 = lists:delete(hd(Params), RList),
					case RList2 of 
						[] -> {UseEffect#unique_effect{in_rage=false,atk_dmg_plus={erlang:max(0,DP-Inc),ADList,[],ZList}}
													  , [?action(?ACTION_RAGE_RMV,Pos,[],0,0,?STATE_DEFAULT)]};
						_ -> {UseEffect#unique_effect{in_rage=false,atk_dmg_plus={erlang:max(0,DP-Inc),ADList,RList2,ZList}},[]}
					end;
                {weary, Inc} ->
					#unique_effect{atk_dmg_sub={ADS,AList,BList}} = UseEffect,
					AList2 = lists:delete(hd(Params), AList),
					case AList2 of
						[] -> {UseEffect#unique_effect{atk_dmg_sub={erlang:max(0,ADS - Inc),[],BList}}
													  , [?action(?ACTION_WEARY_RMV,Pos,[],0,0,?STATE_DEFAULT)]};
						_ -> {UseEffect#unique_effect{atk_dmg_sub={erlang:max(0,ADS - Inc),AList2,BList}}, []}
					end;
                {weak, Inc} ->
                    #unique_effect{damage_plus={DP,AList,BList}} = UseEffect,
					AList2 = lists:delete(hd(Params), AList),
					case AList2 of
						[] -> {UseEffect#unique_effect{damage_plus={erlang:max(0,DP - Inc),[],BList}}
													  , [?action(?ACTION_WEAK_RMV,Pos,[],0,0,?STATE_DEFAULT)]} ;
						_ -> {UseEffect#unique_effect{damage_plus={erlang:max(0,DP - Inc),AList2,BList}},[]} 
					end
            end;
		47 ->
            case SubType of
                {allure, _} ->
                    Action = [?action(?ACTION_ALLURE_RMV_ID,Pos,[],0,0,?STATE_DEFAULT)],
                    {UseEffect, Action};
                {rage, Inc} ->
					 #unique_effect{atk_dmg_plus={DP,ADList,RList,ZList}} = UseEffect,
					ZList2 = lists:delete(hd(Params), ZList),
					case ZList2 of
						[] -> {UseEffect#unique_effect{in_rage=false,atk_dmg_plus={erlang:max(0,DP-Inc),ADList,RList,[]}}
							  , [?action(?ACTION_RAGE_RMV,Pos,[],0,0,?STATE_DEFAULT)]};
						_ -> {UseEffect#unique_effect{in_rage=false,atk_dmg_plus={erlang:max(0,DP-Inc),ADList,RList,ZList2}},[]}
					end;
                {weary, Inc} ->
                    #unique_effect{atk_dmg_sub={ADS,AList,BList}} = UseEffect,
					BList2 = lists:delete(hd(Params), BList),
					case BList2 of
						[] -> {UseEffect#unique_effect{atk_dmg_sub={erlang:max(0,ADS - Inc),AList,[]}}
													  ,[?action(?ACTION_WEARY_RMV,Pos,[],0,0,?STATE_DEFAULT)] };
						_ -> {UseEffect#unique_effect{atk_dmg_sub={erlang:max(0,ADS - Inc),AList,BList2}}, []}
					end;
                {weak, Inc} ->
                    #unique_effect{damage_plus={DP,AList,BList}} = UseEffect,
					BList2 = lists:delete(hd(Params), BList),
					case BList2 of
						[] -> {UseEffect#unique_effect{damage_plus={erlang:max(0,DP - Inc),AList,[]}}
													  ,[?action(?ACTION_WEAK_RMV,Pos,[],0,0,?STATE_DEFAULT)] };
						_ -> {UseEffect#unique_effect{damage_plus={erlang:max(0,DP - Inc),AList,BList2}}, []}
					end
            end;
        29 ->
            #unique_effect{damage_sub={DamageSub,SList}, atk_dmg_plus={ADP,ADList,RList,ZList}} = UseEffect,
            Actions = [?action(?ACTION_DS_RMV,Pos,[],0,0,?STATE_TALENT),?action(?ACTION_ADP_RMV,Pos,[],0,0,?STATE_TALENT)],
            Value = erlang:hd(Args),
            {UseEffect#unique_effect{damage_sub={DamageSub - Value,SList}, atk_dmg_plus={ADP - Value,ADList,RList,ZList}},Actions};
        82 ->
            #unique_effect{atk_dmg_plus={ADP,ADList,RList,ZList}} = UseEffect,
            Actions = [?action(?ACTION_DS_RMV,Pos,[],0,0,?STATE_TALENT),?action(?ACTION_ADP_RMV,Pos,[],0,0,?STATE_TALENT)],
            Value = erlang:hd(Args),
            {UseEffect#unique_effect{atk_dmg_plus={ADP - Value,ADList,RList,ZList}},Actions};
        23 ->
            #unique_effect{damage_sub={DamageSub,SList}} = UseEffect,
            Action = [?action(?ACTION_TR_STONE4,Pos,[],0,0,?STATE_DEFAULT)],
            {UseEffect#unique_effect{damage_sub={DamageSub - erlang:hd(Args),SList}},Action};
		18 ->
			#unique_effect{damage_link1={DamageLink,SrcList}} = UseEffect,
			SrcList2=lists:delete(hd(Params), SrcList),
			case DamageLink of
				0 ->
					{UseEffect,[]};
				_ ->
					case SrcList2 of 
						[] ->Action = [?action(?ACTION_LINK_RMV,Pos,[],0,0,?STATE_TALENT)],
							 delete_damage_link(1,Pos),
							 {UseEffect#unique_effect{damage_link1={0,[]}},Action};
						_ -> {UseEffect#unique_effect{damage_link1={DamageLink,SrcList2}},[]}
					end
			end;
		57 -> 
			#unique_effect{damage_link2={DamageLink,SrcList}} = UseEffect,
			SrcList2=lists:delete(hd(Params), SrcList),
			case DamageLink of
				0 ->
					{UseEffect,[]};
				_ ->
					case SrcList2 of
						[] -> Action = [?action(186,Pos,[],0,0,?STATE_TALENT)],
							  delete_damage_link(2,Pos),
							  {UseEffect#unique_effect{damage_link2={0,[]}},Action};
						_ -> {UseEffect#unique_effect{damage_link2={DamageLink,SrcList2}},[]}
					end
			end;
        85 -> 
            #unique_effect{damage_link3={DamageLink,SrcList}} = UseEffect,
            SrcList2=lists:delete(hd(Params), SrcList),
            case DamageLink of
                0 ->
                    {UseEffect,[]};
                _ ->
                    case SrcList2 of
                        [] -> Action = [?action(267,Pos,[],0,0,?STATE_TALENT)],
                              delete_damage_link(3,Pos),
                              {UseEffect#unique_effect{damage_link3={0,[]}},Action};
                        _ -> {UseEffect#unique_effect{damage_link3={DamageLink,SrcList2}},[]}
                    end
            end;
        33 ->
            {Action, _, UniqueEffect} = morph_sleep_wakeup(Pos, UseEffect),
            {UniqueEffect, Action};
        35 ->
            sub_morph_sing_effect(Pos, UseEffect, SubType,hd(Params));
        36 ->
            sub_morph_sing_effect(Pos, UseEffect, SubType,hd(Params));
        81 -> 
            sub_morph_sing_effect(Pos, UseEffect, SubType,hd(Params));
		39 ->
            {UseEffect#unique_effect{damage_base_blood=0},[]};
		44 ->
			#unique_effect{damage_add={DamageAdd,AcList}} = UseEffect,
			AcList2 = lists:delete(hd(Params), AcList),
			case AcList2 of [] -> {UseEffect#unique_effect{damage_add={max(0,DamageAdd-erlang:hd(Args)),[]}},[?action(174,Pos,[],0,0,?STATE_TALENT)]};
				_ -> {UseEffect#unique_effect{damage_add={max(0,DamageAdd-erlang:hd(Args)),AcList2}},[]}
			end;
		76 ->
			#unique_effect{parry=Parry} = UseEffect,
			case Parry of true -> {UseEffect#unique_effect{parry=false},[?action(258,Pos,[],0,0,?STATE_TALENT)]};
				_ -> {UseEffect,[]}
			end;
		52 ->
			#unique_effect{damage_add={DamageAdd,AcList}} = UseEffect,
			{UseEffect#unique_effect{damage_add={DamageAdd-erlang:hd(Args),AcList}},[]};
		56 ->
			#unique_effect{damage_maxHp=DamageAdd} = UseEffect,
			{UseEffect#unique_effect{damage_maxHp=DamageAdd-hd(Args)},[]};
        _ ->
            {UseEffect,[]}
    end.

%% 检查绝技附加效果时效
check_unique_duration(SrcGerPos) ->
	#unique_buffer{use=UseEffect,effects=Effects} = ?get_unique_buffer(SrcGerPos), 
	{NewUseEffect,NewEffects,RmvActions} =
		lists:foldl(fun(#effect_record{effectId=EffectID,duration=Duration,trSkill=0,subtype=SubType}=EFR,{UseEffectAcc,EffectsAcc,ActionAcc}) ->
							NewDuration = Duration - 1,
							case NewDuration < 0 of
								true ->
									{UseEffectAcc2,Action} = sub_unique_effect(SrcGerPos,UseEffect,EffectID,SubType,[]),
									ActionAcc2 = Action ++ ActionAcc, 
									{UseEffectAcc2,EffectsAcc,ActionAcc2};
								_ ->
									%Action = action_unique_effect(SrcGerPos,EffectID,TrSkill),
									{UseEffectAcc,[EFR#effect_record{duration=NewDuration}|EffectsAcc],ActionAcc}
							end;
					   (#effect_record{}=EFR,{UseEffectAcc,EffectsAcc,ActionAcc}) ->
							{UseEffectAcc,[EFR|EffectsAcc],ActionAcc}
					end,{UseEffect,[],[]},Effects),
	{#unique_buffer{use=NewUseEffect,effects=NewEffects}, RmvActions}.

check_tr_duration(SrcGerPos) ->
	#unique_buffer{use=UseEffect,effects=Effects} = ?get_unique_buffer(SrcGerPos), 
    {NewUseEffect,NewEffects,RmvActions} =
        lists:foldl(fun(#effect_record{trSkill=0}=EFR,{UseEffectAcc,EffectsAcc,ActionAcc}) ->
							{UseEffectAcc,[EFR|EffectsAcc],ActionAcc};
					   (#effect_record{effectId=EffectID,duration=Duration,trSkill=TrSkill}=EFR,{UseEffectAcc,EffectsAcc,ActionAcc}) ->
                        NewDuration = Duration - 1,
                        case NewDuration < 0 of
                            true ->
                                {UseEffectAcc2,Action} = sub_tr_effect(SrcGerPos,UseEffect,EffectID,TrSkill),
                                ActionAcc2 = 
                                    case Action of
                                        undefined ->
                                            ActionAcc;
                                        _ ->
                                            [Action|ActionAcc]
                                    end,
                                {UseEffectAcc2,EffectsAcc,ActionAcc2};
                            _ ->
								%Action = action_unique_effect(SrcGerPos,EffectID,TrSkill),
                                {UseEffectAcc,[EFR#effect_record{duration=NewDuration}|EffectsAcc],ActionAcc}
                        end
                    end,{UseEffect,[],[]},Effects),
    ?set_unique_buffer(SrcGerPos,#unique_buffer{use=NewUseEffect,effects=NewEffects}), 
	RmvActions.

%% 通过进程字段来筛选出还存活的对象
filter_alive_list(PosList) ->
    lists:foldl(fun(E,AliveAcc) -> 
                    #ger{gerHp=GerHp} = erlang:get(E),
                    case GerHp > 0 of
                        true ->
                            [E|AliveAcc];
                        _ ->
                            AliveAcc
                    end
                end,[],PosList).

%% 这里处理追加、减怒气、眩晕等类型(减怒气也能在attack2里面做)
%% 这里实际Unhandleds的最后应该是最先需要处理的,因此这里处理完后,
%% 不需要对结果进行倒序 
%% TarPosList 给选择眩晕对象使用
deal_c1_unhandledIs(SrcPos,DataSkill,#uh{c1=C1}) ->
	{C1_2,_} = lists:foldl(fun({EffectID,TarPos,SrcGerPos},{Acc,LastID}) ->
							   if EffectID == LastID -> {[{EffectID,TarPos,SrcGerPos,0}|Acc],EffectID};
								  true -> {[{EffectID,TarPos,SrcGerPos,1}|Acc],EffectID}
							   end
					   end, {[],0}, lists:keysort(1, C1)),
	deal_unhandledIs(lists:reverse(C1_2),SrcPos,DataSkill,[]).

deal_c2_unhandledIs(SrcPos,DataSkill,#uh{c2=C2}) ->
	{C2_2,_} = lists:foldl(fun({EffectID,TarPos,SrcGerPos},{Acc,LastID}) ->
							   if EffectID == LastID -> {[{EffectID,TarPos,SrcGerPos,1}|Acc],EffectID};
								  true -> {[{EffectID,TarPos,SrcGerPos,0}|Acc],EffectID}
							   end
					   end, {[],0}, lists:keysort(1, C2)),
	deal_unhandledIs(lists:reverse(C2_2),SrcPos,DataSkill,[]).

deal_unhandledIs([],_,_,Acc) ->
    rbuild_unhandled_action(Acc,[],[]);
deal_unhandledIs([{EffectID,TarPos,SrcGerPos,IsDisPlayAction}|T],SrcPos,DataSkill,Acc) ->
    #data_unique_effect{apply_type=ApplyType,duration=Duration,args=Args,apply_target=ApplyTarget} = data_unique_effect:get(EffectID),
    NewAcc = 
    case ApplyType of
		27 ->
			ConfuseList = if IsDisPlayAction == 1 -> [] ; true -> [SrcPos] end,
			#attack_info{uaction=CAction}=do_confuse(SrcGerPos, TarPos, EffectID, Duration, Args, #attack_info{confuse_list=ConfuseList}),
			CAction++Acc;
        18 ->
			MissList = get_miss_list(),
			TarPos2 = lists:filter(fun(TP)-> #ger{gerHp=TGerHp} = ?get_ger(TP),
											 #unique_buffer{use=#unique_effect{invinciblep=Invi}}
															   =?get_unique_buffer(TP), 
											 IsMiss = lists:member(TP,MissList),
											 TGerHp > 0 andalso not(IsMiss) andalso not(Invi) end, TarPos),
            lists:foldl(fun(E,Acc2) ->
                            #ger{gerHp=GerHp} = ?get_ger(E),
                            case GerHp =< 0 of
                                true ->
                                    Acc2;
                                _ ->
                                    case update_target_maimed(SrcPos,E,?ACTION_LINK_RMV,Duration,EffectID) of
                                        invinciblep ->
                                            Acc2;
										_ ->
											mark_damage_link(1,E,TarPos2),
											UniqueBuffer = #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(E),
											#unique_effect{damage_link1={_,LinkList}}=UniqueEffect,
											UniqueEffect2 = UniqueEffect#unique_effect{damage_link1={erlang:hd(Args)
																									 ,[SrcGerPos|lists:delete(SrcGerPos, LinkList)]}},
											?set_unique_buffer(E, UniqueBuffer#unique_buffer{use=UniqueEffect2}), 
											[?action(117,E,[],0,0,?STATE_TALENT)|Acc2]
									end
							end
						end,Acc,TarPos2);
		57 ->
			MissList = get_miss_list(),
			TarPos2 = lists:filter(fun(TP2)-> #ger{gerHp=TGerHp} = ?get_ger(TP2),
											 #unique_buffer{use=#unique_effect{invinciblep=Invi}}
															   =?get_unique_buffer(TP2), 
											 IsMiss = lists:member(TP2,MissList),
											 TGerHp > 0 andalso not(IsMiss) andalso not(Invi) end, TarPos),
            lists:foldl(fun(E,Acc2) ->
                            #ger{gerHp=GerHp} = ?get_ger(E),
                            case GerHp =< 0 of
                                true ->
									Acc2;
								_ ->
									case update_target_maimed(SrcPos,E,186,Duration,EffectID) of
										invinciblep ->
											Acc2;
										_ ->
											mark_damage_link(2,E,TarPos2),
											UniqueBuffer = #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(E),
											#unique_effect{damage_link2={_,LinkList}}=UniqueEffect,
											UniqueEffect2 = UniqueEffect#unique_effect{damage_link2={erlang:hd(Args)
																									 ,[SrcGerPos|lists:delete(SrcGerPos, LinkList)]}},
											?set_unique_buffer(E, UniqueBuffer#unique_buffer{use=UniqueEffect2}), 
											[?action(185,E,[],0,0,?STATE_TALENT)|Acc2]
									end
							end
                        end,Acc,TarPos2);
         85 ->
            MissList = get_miss_list(),
            TarPos2 = lists:filter(fun(TP2)-> #ger{gerHp=TGerHp} = ?get_ger(TP2),
                                             #unique_buffer{use=#unique_effect{invinciblep=Invi}}
                                                               =?get_unique_buffer(TP2), 
                                             IsMiss = lists:member(TP2,MissList),
                                             TGerHp > 0 andalso not(IsMiss) andalso not(Invi) end, TarPos),
            lists:foldl(fun(E,Acc2) ->
                            #ger{gerHp=GerHp} = ?get_ger(E),
                            case GerHp =< 0 of
                                true ->
                                    Acc2;
                                _ ->
                                    case update_target_maimed(SrcPos,E,186,Duration,EffectID) of
                                        invinciblep ->
                                            Acc2;
                                        _ ->
                                            mark_damage_link(3,E,TarPos2),
                                            UniqueBuffer = #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(E),
                                            #unique_effect{damage_link3={_,LinkList}}=UniqueEffect,
                                            UniqueEffect2 = UniqueEffect#unique_effect{damage_link3={erlang:hd(Args)
                                                                                                     ,[SrcGerPos|lists:delete(SrcGerPos, LinkList)]}},
                                            ?set_unique_buffer(E, UniqueBuffer#unique_buffer{use=UniqueEffect2}), 
                                            [?action(266,E,[],0,0,?STATE_TALENT)|Acc2]
                                    end
                            end
                        end,Acc,TarPos2);
		52 ->
			UniqueBuffer = #unique_buffer{use=UseEffect,effects=Effects} = ?get_unique_buffer(TarPos),
			{NewUseEffect,Action} = add_unique_effect(0,TarPos,UseEffect,ApplyType,Args,Duration,EffectID),
			?set_unique_buffer(TarPos,UniqueBuffer#unique_buffer{use=NewUseEffect,effects=[#effect_record{effectId=EffectID,duration=Duration}|Effects]}),
			Action++Acc;
		_ ->
            TarGerPos = 
                %% 眩晕的参数是个列表,需要单独处理下
                case ApplyType =:= 6 of
                    true ->
                        %% 只在活人中随机
                        case filter_alive_list(TarPos) of
                            [] ->
                                %% 全死了就不处理这个了
                                undefined;
                            AliveList ->
                                erlang:hd(random_select_list(1,AliveList))
                        end;
                    _ ->
                        TarPos
                end,
            case TarGerPos of
                undefined ->
                    deal_unhandledIs(T,SrcPos,DataSkill,Acc);
                _ ->
                    TarGer = #ger{gerHp=GerHp,gerSp=GerSp,gerAttr=#gerAttr{gerSpMax=SpMax}} = erlang:get(TarGerPos),
                    %% 死了就不放动画了
                    case GerHp =< 0 of
                        true ->
                            Acc;
                        _ ->
                            case ApplyType of
                                5 ->
                                    SpSub = erlang:hd(Args),
                                    case sub_sp(TarGerPos,TarGer,SpSub) of
                                        invinciblep ->
                                            Acc;
                                        _ ->
                                            %% 103 减怒气
                                            [?action(103,TarGerPos,[],SpSub,0,?STATE_TALENT)|Acc]
                                    end;
                                6 ->
                                	TarGer = ?get_ger(TarGerPos),
                                    A1 = case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
                                             false-> case update_target_maimed(SrcPos,TarGerPos,6,Duration) of
                                                     invinciblep -> Acc;
                                                     _ -> [?action(6,TarGerPos,[],0,0,?STATE_REEL)|Acc]
                                                 end;
                                             true-> [?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
                                         end,
                                    case ApplyTarget of 
                                        {_,2} ->
                                            TarGerPos2 = case filter_alive_list(TarPos) -- [TarGerPos] of
                                                             [] ->  undefined;
                                                             AliveList1 -> erlang:hd(random_select_list(1,AliveList1))
                                                         end,
                                            case TarGerPos2 of 
                                                ?undefined ->  A1 ;
                                                _ -> TarGer2 = ?get_ger(TarGerPos2),
                                                     case is_Boss_fight_type(TarGer2#ger.gerBase#gerBase.gerTypeID) of
                                                         false-> case update_target_maimed(SrcPos,TarGerPos2,6,Duration) of
                                                                     invinciblep ->  A1;
                                                                     _ -> [?action(6,TarGerPos2,[],0,0,?STATE_REEL)|A1]
                                                     end;
                                                         true-> [?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
                                                     end
                                            end;
                                        _ -> A1
                                    end;
								72 ->
									TarGer = ?get_ger(TarGerPos),
									case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
										false->
											case hd(Args) of
												1 ->case update_target_maimed(SrcPos,TarGerPos,232,Duration) of
														invinciblep ->   Acc;
														_ ->X1Action = if IsDisPlayAction == 1 ->[?action(230,SrcGerPos,[],0,0,?STATE_TALENT)];true -> [] end, 
															[?action(232,TarGerPos,[],0,0,?STATE_TALENT),?action(231,TarGerPos,[],0,0,?STATE_TALENT)]
															  ++X1Action++Acc
													end;
												2 ->case update_target_maimed(TarGerPos,TarGerPos,232,Duration) of
														invinciblep -> Acc;
														_ -> [?action(232,TarGerPos,[],0,0,?STATE_TALENT),?action(231,TarGerPos,[],0,0,?STATE_TALENT)
															  ,?action(250,SrcGerPos,[],0,0,?STATE_TALENT)]++Acc
													end
											end;
										true->
											[?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
									end;
                                9 ->
                                        SpAdd = erlang:hd(Args),
                                        erlang:put(TarGerPos,TarGer#ger{gerSp=min(SpMax,GerSp + SpAdd)}),
                                        %% 加怒气
									case EffectID of 1010 ->  [?action(113,TarGerPos,[],SpAdd,0,?STATE_TALENT)|Acc];
													 _ ->  [?action(113,TarGerPos,[],0,SpAdd,?STATE_TALENT)|Acc]
									end;
                                25 ->   
                                		TarGer = ?get_ger(TarGerPos),
                            			case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
                            				false->
                                                case update_target_maimed(SrcPos,TarGerPos,?ACTION_FROZEN_ID,Duration) of
                                                    invinciblep ->
                                                        Acc;
                                                    _ ->
                                               	 	    [?action(?ACTION_FROZEN_ID,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
                                                end;
                                            true->
                                               	 [?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
                                        end;
								71 ->   
									TarGer = ?get_ger(TarGerPos),
									case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
										false->
											case hd(Args) of
												1 ->
													case update_target_maimed(SrcPos,TarGerPos,?ACTION_FROZEN_ID_1,Duration) of
														invinciblep -> Acc;
														_ -> X1Action = if IsDisPlayAction == 1 ->[?action(234,SrcGerPos,[],0,0,?STATE_TALENT)];true -> [] end, 
															 [?action(?ACTION_FROZEN_ID_1,TarGerPos,[],0,0,?STATE_DEFAULT),?action(235,TarGerPos,[],0,0,?STATE_DEFAULT)]
															  ++X1Action++Acc
													end;
												2 ->case update_target_maimed(TarGerPos,TarGerPos,?ACTION_FROZEN_ID_1,Duration) of
														invinciblep -> Acc;
														_ -> [?action(?ACTION_FROZEN_ID_1,TarGerPos,[],0,0,?STATE_DEFAULT),?action(235,TarGerPos,[],0,0,?STATE_DEFAULT)
															  ,?action(251,SrcGerPos,[],0,0,?STATE_DEFAULT)]++Acc
													end
											end;
										true->
											[?action(120,TarGerPos,[],0,0,?STATE_DEFAULT)|Acc]
									end;
                                26 ->
                                		TarGer = ?get_ger(TarGerPos),
                            			case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
                            				false->
                                                case update_target_maimed(SrcPos,TarGerPos,?ACTION_PARALYSIS_ID,Duration) of
                                                    invinciblep ->
                                                        Acc;
                                                    _ ->
                                                	    [?action(?ACTION_PARALYSIS_ID,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
                                                end;
                                            true->
                                                [?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
                                        end;
								73 ->
									TarGer = ?get_ger(TarGerPos),
									case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
										false->
											case hd(Args) of
												1 ->
													case update_target_maimed(SrcPos,TarGerPos,?ACTION_PARALYSIS_ID_1,Duration) of
														invinciblep -> Acc;
														_ -> X1Action = if IsDisPlayAction == 1 ->[?action(246,SrcGerPos,[],0,0,?STATE_TALENT)];true -> [] end, 
															 [?action(?ACTION_PARALYSIS_ID_1,TarGerPos,[],0,0,?STATE_TALENT),?action(247,TarGerPos,[],0,0,?STATE_TALENT)]
															  ++X1Action++Acc
													end;
												2 ->
													case update_target_maimed(TarGerPos,TarGerPos,?ACTION_PARALYSIS_ID_1,Duration) of
														invinciblep -> Acc;
														_ -> [?action(?ACTION_PARALYSIS_ID_1,TarGerPos,[],0,0,?STATE_TALENT),?action(247,TarGerPos,[],0,0,?STATE_TALENT)
															  ,?action(254,SrcGerPos,[],0,0,?STATE_TALENT)]++Acc
													end
											end;
										true->
											[?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
									end;
								74 ->
									TarGer = ?get_ger(TarGerPos),
									case is_Boss_fight_type(TarGer#ger.gerBase#gerBase.gerTypeID) of
										false->
											case hd(Args) of
												1 ->
													case update_target_maimed(SrcPos,TarGerPos,240,Duration) of
														invinciblep -> Acc;
														_ -> X1Action = if IsDisPlayAction == 1 ->[?action(238,SrcGerPos,[],0,0,?STATE_TALENT)];true -> [] end, 
															 [?action(240,TarGerPos,[],0,0,?STATE_TALENT),?action(239,TarGerPos,[],0,0,?STATE_TALENT)]
															  ++X1Action++Acc
													end;
												2 ->
													case update_target_maimed(TarGerPos,TarGerPos,240,Duration) of
														invinciblep -> Acc;
														_ -> [?action(240,TarGerPos,[],0,0,?STATE_TALENT),?action(239,TarGerPos,[],0,0,?STATE_TALENT)
															  ,?action(252,SrcGerPos,[],0,0,?STATE_TALENT)]++Acc
													end
											end;
										true->
											[?action(120,TarGerPos,[],0,0,?STATE_TALENT)|Acc]
									end;
                                _->
                                        Acc
                            end
                        end 
            end 
    end,
    deal_unhandledIs(T,SrcPos,DataSkill,NewAcc).

rbuild_unhandled_action([],T1,T2) -> lists:reverse(T2 ++ T1);
rbuild_unhandled_action([#p_action{actionID=232}=A,#p_action{actionID=231}=B,#p_action{actionID=250}=C|T],T1,T2) -> rbuild_unhandled_action(T,[C,A,B]++T2 ++ T1,[]);
rbuild_unhandled_action([#p_action{actionID=232}=A,#p_action{actionID=231}=B|T],T1,T2) -> rbuild_unhandled_action(T,[A|T1],[B|T2]);
rbuild_unhandled_action([#p_action{actionID=236}=A,#p_action{actionID=235}=B,#p_action{actionID=251}=C|T],T1,T2) -> rbuild_unhandled_action(T,[C,A,B]++T2 ++ T1,[]);
rbuild_unhandled_action([#p_action{actionID=236}=A,#p_action{actionID=235}=B|T],T1,T2) -> rbuild_unhandled_action(T,[A|T1],[B|T2]);
rbuild_unhandled_action([#p_action{actionID=240}=A,#p_action{actionID=239}=B,#p_action{actionID=252}=C|T],T1,T2) -> rbuild_unhandled_action(T,[C,A,B]++T2 ++ T1,[]);
rbuild_unhandled_action([#p_action{actionID=240}=A,#p_action{actionID=239}=B|T],T1,T2) -> rbuild_unhandled_action(T,[A|T1],[B|T2]);
rbuild_unhandled_action([#p_action{actionID=244}=A,#p_action{actionID=243}=B,#p_action{actionID=253}=C|T],T1,T2) -> rbuild_unhandled_action(T,[C,A,B]++T2 ++ T1,[]);
rbuild_unhandled_action([#p_action{actionID=244}=A,#p_action{actionID=243}=B|T],T1,T2) -> rbuild_unhandled_action(T,[A|T1],[B|T2]);
rbuild_unhandled_action([#p_action{actionID=248}=A,#p_action{actionID=247}=B,#p_action{actionID=254}=C|T],T1,T2) -> rbuild_unhandled_action(T,[C,A,B]++T2 ++ T1,[]);
rbuild_unhandled_action([#p_action{actionID=248}=A,#p_action{actionID=247}=B|T],T1,T2) -> rbuild_unhandled_action(T,[A|T1],[B|T2]);
rbuild_unhandled_action([X|T],T1,T2) -> rbuild_unhandled_action(T,[X|T2]++T1,[]).

%% 判断追加攻击的类型和技能
get_append_attack_info(_,_,_,[],_,_) ->
    {undefined,undefined,undefined,[],[]};
get_append_attack_info(SrcPos,Skills,UniqueSkill,[H|T],LastType,Count) ->
    %% 不管配置怎么配,要求的是普通追加后只能再追加一次.死亡后不进行追加
    #unique_buffer{use=UniqueBuffer} =Buffer = ?get_unique_buffer(SrcPos),
    case is_dead(SrcPos) orelse (LastType == normal andalso Count >= 2) 
		orelse (LastType == normal_4 andalso Count >= 2) 
		orelse (LastType == normal_3 andalso Count >= 1)
        orelse (LastType == normal_5 andalso Count >= 1)
		orelse (LastType == normal_crazy andalso Count >= 3) 
		orelse (LastType == unique_noSp_3 andalso Count >= 4)
		orelse (LastType == unique_noSp_4 andalso Count >= 1)
		of
		true ->			
			UniqueBuff2 = UniqueBuffer#unique_effect{damage_double=0},
			?set_unique_buffer(SrcPos,Buffer#unique_buffer{use=UniqueBuff2}),
			{undefined,undefined,undefined,[],[]};
        _ ->
            %% 获得追击的类型和对象
            #append_attack{type=Type, target=TargetList,args=AArg} = H,
                %% 追加最多3次,第4次进行一次普攻
                %CCMax = case lists:keyfind(max_c, 1, AArg) of false -> ?UNIQURE_APPEND_LIMIT ; {_,N} -> N end,
                #unique_effect{maxCount=CCMax} = UniqueBuffer,
            {Skill, Action} = 
                case Count =:= CCMax  andalso LastType /= normal_2 andalso LastType /= unique_noSp of
                    true ->
                        #gerSkill{normal=NormalSkills} = Skills,
                        NormalSkill = data_skill:get(random_one_skill(NormalSkills)),
                        {NormalSkill, []};
                    _ ->
                        case Type of
                            unique ->
								case AArg of 
									[] -> {UniqueSkill,[?action(110,SrcPos,[],0,0,?STATE_DEFAULT)]};
									1 -> {UniqueSkill,[?action(175,SrcPos,[],0,0,?STATE_DEFAULT)]};
									2 -> {UniqueSkill,[?action(168,SrcPos,[],0,0,?STATE_DEFAULT)]};
                                    _ -> {UniqueSkill,[?action(110,SrcPos,[],0,0,?STATE_DEFAULT)]}
									%3 -> {UniqueSkill,[?action(213,SrcPos,[],0,0,?STATE_DEFAULT)]}
								end;									
%% 							unique3 ->
%% 								{UniqueSkill, [?action(168,SrcPos,[],0,0,?STATE_DEFAULT)]};
                            unique_critic ->
                                {UniqueSkill,[?action(137,SrcPos,[],0,0,?STATE_DEFAULT)]};
							unique_critic_2 ->
								{UniqueSkill, [?action(165, SrcPos,[],0,0,?STATE_DEFAULT)]};
							unique_critic_3 ->
								{UniqueSkill,[?action(169,SrcPos, [],0,0,?STATE_DEFAULT)]};
							unique_noSp ->
								#gerSkill{unique=UniqueSkills} = Skills,
								XSkill = data_skill:get(random_one_skill(UniqueSkills)),
								{XSkill,[?action(170,SrcPos,[],0,0,?STATE_DEFAULT)]};
							unique_noSp_2 ->
								#gerSkill{unique=UniqueSkills} = Skills,
								XSkill = data_skill:get(random_one_skill(UniqueSkills)),
								{XSkill,[?action(181,SrcPos,[],0,0,?STATE_DEFAULT)]};
							unique_noSp_3 ->
									   #gerSkill{unique=UniqueSkills} = Skills,
									   XSkill = data_skill:get(random_one_skill(UniqueSkills)),
									   SkillActionID = 213 + Count,
									   {XSkill,[?action(SkillActionID,SrcPos,[],0,0,?STATE_DEFAULT)]};
							unique_noSp_4 ->
								#gerSkill{unique=UniqueSkills} = Skills,
								XSkill = data_skill:get(random_one_skill(UniqueSkills)),
								{XSkill,[?action(222,SrcPos,[],0,0,?STATE_DEFAULT)]};
							_ ->
                                #gerSkill{normal=NormalSkills} = Skills,
                                NormalSkill = data_skill:get(random_one_skill(NormalSkills)),
                                ActionID = 
                                    case Type of
                                        hit_critic ->		108;
                                        speed ->			45;
                                        normal_dot ->		136;
										normal_crazy->		178;
										normal_2 ->			171;
										normal_3 ->			162;
										normal_4 ->			if Count < 1 -> 166; true -> 167 end;
                                        normal_5 ->         274;
                                        _ ->				[_,ActionType] = AArg, 
															case ActionType of 1 -> 109; 2 -> 197;3->273;4->270 end
                                    end,
                                {NormalSkill,[?action(ActionID,SrcPos,[],0,0,?STATE_DEFAULT)]} 
                        end 
                end,
            TargetList2 = 
                case Type of
                    unique ->
                        case AArg of
                            3 -> TargetList;
                            _ -> select_target(Skill,SrcPos)
                        end;
                    normal_crazy -> select_target(Skill,SrcPos); 
                    _ -> TargetList
                end,
            %% 这里需要判断被攻击的是否还活着,因为追击判定时对方可能还活着,但是打完后可能已经死了
            case select_unique_append_alive(TargetList2,Skill,SrcPos) of
                [] ->
                    get_append_attack_info(SrcPos,Skills,UniqueSkill,T,LastType,Count); 
                List ->
                    {H,List,Skill,Action,T}
            end
    end.

%% 减怒气 
sub_sp(SrcPos, SrcGer, Inc) ->
    #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(SrcPos),
    #unique_effect{invinciblep=Invinciblep} = UniqueBuffer,
    case Invinciblep of
        true ->
            invinciblep;
        _ ->
            erlang:put(SrcPos, SrcGer#ger{gerSp=max(0,SrcGer#ger.gerSp-Inc)}) 
    end.

update_append_list(#attack_info{append=Append} = AttackInfo, Type, ApplyList, AlreadyAcc, Args) ->
    Args2 = 
        case Args of
            [] ->
                [];
            [H|_] ->
                H
        end,
    Append2 = [#append_attack{type=Type, target = ApplyList, args=Args2}|Append],
    AttackInfo#attack_info{append=Append2, trigger=AlreadyAcc}.

get_dot_rmvAction(RMVInfo,NewDot,Pos)->
	lists:foldl(fun(ActionType,ActionAcc) ->
						case lists:keyfind(ActionType, 4, NewDot) of
							false -> [get_dot_rmv_action(ActionType,Pos)|ActionAcc];
							_ -> ActionAcc
						end	
				end, [], lists:foldl(fun(ID,Acc)->
											 case lists:member(ID, Acc) of true -> Acc;_ -> [ID|Acc] end
									 end , [], RMVInfo)).

%% 判断dot伤害(因为dot和其他技能有效判断的时机不一样,所以只能单独来检测时间了)
check_dot(Pos,#attack_info{action=ActionList} = Acc) ->
    Ger = ?get_ger(Pos),
    #unique_buffer{use=UniqueEffect} = UniqueBuffer = ?get_unique_buffer(Pos),
    #unique_effect{dot=Dot,invinciblep=Invinciblep} = UniqueEffect,
    case Dot =:= [] of
        true ->
            Acc;
        _ ->
            %%最终被谁击杀目前的条件来讲不重要,这里返回击杀者,新的链表,和总伤害
%%             {SrcGerPos,NewDot,DamageT,DotRMVAction} = 
%%                 lists:foldl(fun({SrcGerPosT,DotD,Duration,ActionType},{_,DotAcc,DamageAcc,DotActionAcc}) ->
%%                                 NewDuration = Duration - 1,
%%                                 NewDamageAcc = DamageAcc + DotD,
%%                                 case NewDuration of
%%                                     0 -> {SrcGerPosT,DotAcc,NewDamageAcc,[get_dot_rmv_action(ActionType,Pos)|DotActionAcc]};
%%                                     _ -> {SrcGerPosT,[{SrcGerPosT,DotD,NewDuration,ActionType}|DotAcc],NewDamageAcc,DotActionAcc}
%%                                 end
%%                             end,{0,[],0,[]},Dot),
			{SrcGerPos,NewDot,DamageT,RMVInfo} = 
				lists:foldl(fun({SrcGerPosT,DotD,Duration,ActionType},{_,DotAcc,DamageAcc,RMVInfoAcc}) ->
								   NewDuration = Duration - 1,
								   damage_stastic(SrcGerPosT,DotD),
								   case NewDuration of 0 -> {SrcGerPosT,DotAcc,DamageAcc + DotD,[ActionType|RMVInfoAcc]};
									   _ -> {SrcGerPosT,[{SrcGerPosT,DotD,NewDuration,ActionType}|DotAcc],DamageAcc+DotD,RMVInfoAcc}
								   end end,{0,[],0,[]}, Dot),
			DotRMVAction = get_dot_rmvAction(RMVInfo,NewDot,Pos),
            UniqueEffect2 = UniqueEffect#unique_effect{dot=NewDot},
            Damage0 =
                case Invinciblep of
                    true ->
                        0;
                    _ ->
                        DamageT
                end,
            #ger{gerHp=GerHp,gerProHp=GerProHp} = Ger,
            ?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2}),
			{Inv0,Damage} = check_d_minHp(Pos,Damage0),
			InvAction = if Inv0 -> set_d_inv(); true -> [] end,
            {ProHpDecf,HpDecf} = decf_ger_hp(GerProHp, Damage),
            ActionList2 = InvAction ++ [?pro_action(23,Pos,[],HpDecf,ProHpDecf,0,?STATE_DEFAULT)|ActionList],
            ActionList3 = DotRMVAction ++ ActionList2,
            GerHp2 = add_hp(GerHp, ?a(Ger, gerHpMax), HpDecf),
            GerProHp2 = GerProHp + ProHpDecf,
            Ger2 = Ger#ger{gerHp=GerHp2,gerProHp=GerProHp2},
            
            #attack_info{action=ActionList4} = Acc2 = do_damage_link(SrcGerPos,Pos,Acc#attack_info{action=ActionList3,damage=Damage}),
			?set_ger(Ger2),
            case GerHp2 =< 0 of
                true ->
					{IsReborn,RebornAction,KillHooks} = do_reborn(SrcGerPos,Pos,Ger2,{0,[],[]}),
		            if IsReborn == false ->
						  StandFellowAction = stand_fellow(Pos),
				        Acc2#attack_info{action=StandFellowAction++KillHooks ++ [?action(?ACTION_DEAD,Pos,[],0,0,?STATE_DEFAULT)|ActionList4]};
			        true ->
				        Acc2#attack_info{action=RebornAction ++ ActionList4}  
		            end;
                _ ->
                    Acc2
            end
    end.

%% 添加伤害链接
mark_damage_link(Type,Pos, ApplyList) ->
    {LinkA,LinkB,LinkC} = ?get_damage_link(Pos),
	LinkN = 
		case Type of 
			1 -> {mark_damage_link2(Pos,ApplyList,LinkA),LinkB,LinkC};
			2 -> {LinkA,mark_damage_link2(Pos,ApplyList,LinkB),LinkC};
            3 -> {LinkA,LinkB,mark_damage_link2(Pos,ApplyList,LinkC)}
		end,
	?set_damage_link(Pos, LinkN).
mark_damage_link2(Pos,ApplyList,Link) ->
        lists:foldl(fun(E, Acc) ->
                        case lists:member(E, Acc) of
                            false ->
                                case E =:= Pos of
                                    true ->
                                        Acc;
                                    _ ->
                                        [E|Acc] 
                                end;
                            _ ->
                                Acc
                        end
                    end, Link, ApplyList).
    

%% 取消伤害连接
delete_damage_link(Type,Pos) ->
    {LinkA,LinkB,LinkC} = ?get_damage_link(Pos), 
	LinkN = case Type of
				1 -> {delete_damage_link2(Pos,LinkA),LinkB,LinkC};
				2 -> {LinkA,delete_damage_link2(Pos,LinkB),LinkC};
                3 -> {LinkA,LinkB,delete_damage_link2(Pos,LinkC)}
			end,
    ?set_damage_link(Pos,LinkN).

delete_damage_link2(Pos,Link) ->
	    lists:foreach(fun(E) ->
                    {LinkE1,LinkE2,LinkE3} = ?get_damage_link(E),
                    ?set_damage_link(E, {lists:delete(Pos, LinkE1),lists:delete(Pos,LinkE2),lists:delete(Pos,LinkE3)})
                end, Link),
		[].


%% 处理伤害链接
do_damage_link(AtkPos,Pos,#attack_info{damage=Damage}=AtkInfo) -> 
    case ?get_damage_link(Pos) of
        {[],[],[]} ->
            AtkInfo;
        {LinkA,LinkB,LinkC} ->
            #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(Pos),
            #unique_effect{damage_link1={DamageLink1,_},damage_link2={DamageLink2,_},damage_link3={DamageLink3,_},damage_dec=DamageDec} = UniqueBuffer,
            HarmValue0_1 = erlang:trunc(Damage * DamageLink1 / 100 * (1 - DamageDec / 10000) ),
			HarmValue0_2 = erlang:trunc(Damage * DamageLink2 / 100 * (1 - DamageDec / 10000) ),
            HarmValue0_3 = erlang:trunc(Damage * DamageLink3 / 100 * (1 - DamageDec / 10000) ),
			Acc1 = do_damage_link_calc(AtkPos,LinkA,HarmValue0_1,AtkInfo),
			Acc2 = do_damage_link_calc(AtkPos,LinkB,HarmValue0_2,Acc1),
            Acc3 = do_damage_link_calc(AtkPos,LinkC,HarmValue0_3,Acc2),
			Acc3
	end.
do_damage_link_calc(AtkPos,Link,HarmValue0,AtkInfo) ->
            lists:foldl(fun(E, #attack_info{action=ActionList}=Acc) ->
                            #ger{gerSp=TarSp,gerHp=TarGerHp,gerAttr=#gerAttr{gerSpMax=GerSpMax},gerProHp=TarProHp} = Targer = ?get_ger(E),
                            #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(E),
                            #unique_effect{invinciblep=Invinciblep,spRa=SpRa} = UniqueEffect,
                            case Invinciblep of
                                true ->
                                    Acc#attack_info{action=[?action(?ACTION_NORMAL_HURT,E,[],0,0,?STATE_DEFAULT)|ActionList]};
                                _ ->
                                    TarAddSp = trunc(real_add_sp(TarSp,?HURT_ADD_SP,GerSpMax) * SpRa / 100),
                                    TarSp2 = TarSp + TarAddSp,
									{Inv0,HarmValue} = check_d_minHp(E,HarmValue0),
									InvAction = if Inv0 -> set_d_inv(); true -> [] end,
                                    {ProHpDecf,HpDecf} = decf_ger_hp(TarProHp, HarmValue),
                                    TarGerHp2 = TarGerHp + HpDecf,
                                    TarProHp2 = TarProHp + ProHpDecf,
                                    Targer2 = Targer#ger{gerHp=TarGerHp2,gerSp=TarSp2,gerProHp=TarProHp2},
                                    ?set_ger(Targer2),
									LinkAction = ?pro_action(?ACTION_NORMAL_HURT,E,[],HpDecf,ProHpDecf,TarAddSp,?STATE_DEFAULT),
									HurtAction = 
										case TarGerHp2 =< 0 of
											true ->
												{IsReborn,RebornAction2,SrcKillHooks} = do_reborn(AtkPos,E,Targer2,{0,[],[]}),
												case IsReborn of
													false ->
														StandFellowAction = stand_fellow(E),
														StandFellowAction++SrcKillHooks ++ [?action(?ACTION_DEAD,E,[],0,0,?STATE_DEFAULT)] ;
													_ ->
														RebornAction2 
												end;
											_ ->
												[]
										end,
									Acc#attack_info{action=HurtAction ++ InvAction ++[LinkAction|ActionList]} 
							end
						end,AtkInfo,Link) .

%%返回连接生成的action列表
do_damage_link2(AtkPos,Pos,#attack_info{damage=Damage}=AtkInfo)  when Damage > 0 -> 
    case ?get_damage_link(Pos) of
        {[],[],[]} ->
            {[],AtkInfo};
        {LinkA,LinkB,LinkC} ->
			#unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(Pos),
            #unique_effect{damage_link1={DamageLink1,_},damage_link2={DamageLink2,_},damage_link3={DamageLink3,_},damage_dec=DamageDec} = UniqueBuffer,
            HarmValue0_1 = erlang:trunc(Damage * DamageLink1 / 100 * (1 - DamageDec / 10000) ),
			HarmValue0_2 = erlang:trunc(Damage * DamageLink2 / 100 * (1 - DamageDec / 10000) ),
            HarmValue0_3 = erlang:trunc(Damage * DamageLink3 / 100 * (1 - DamageDec / 10000) ),
			{Action1,Acc1} = do_damage_link2_calc(AtkPos,LinkA,HarmValue0_1,AtkInfo),
			{Action2,Acc2} = do_damage_link2_calc(AtkPos,LinkB,HarmValue0_2,Acc1),
            {Action3,Acc3} = do_damage_link2_calc(AtkPos,LinkC,HarmValue0_3,Acc2),
			{Action1 ++ Action2++Action3,Acc3}
	end;
do_damage_link2(_,_,AtkInfo) -> {[],AtkInfo}.
do_damage_link2_calc(AtkPos,Link,HarmValue0,AtkInfo) ->
            lists:foldl(fun(E, {ActionAcc,#attack_info{action=_ActionList}=Acc}) ->
                            #ger{gerSp=TarSp,gerHp=TarGerHp,gerAttr=#gerAttr{gerSpMax=GerSpMax},gerProHp=TarProHp} = Targer = ?get_ger(E),
                            #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(E),
                            #unique_effect{invinciblep=Invinciblep, spRa=SpRa} = UniqueEffect,
                            case Invinciblep of
                                true ->
                                    {[?action(?ACTION_NORMAL_HURT,E,[],0,0,?STATE_DEFAULT)|ActionAcc],Acc};
                                _ ->
                                    TarAddSp = trunc(real_add_sp(TarSp,?HURT_ADD_SP,GerSpMax) *SpRa / 100),
                                    TarSp2 = TarSp + TarAddSp,
									{Inv0,HarmValue} = check_d_minHp(E,HarmValue0),
									InvAction = if Inv0 -> set_d_inv(); true -> [] end,
                                    {ProHpDecf,HpDecf} = decf_ger_hp(TarProHp, HarmValue),
                                    TarGerHp2 = add_hp(TarGerHp, ?a(Targer, gerHpMax), HpDecf),
                                    TarProHp2 = TarProHp + ProHpDecf,
                                    Targer2 = Targer#ger{gerHp=TarGerHp2,gerSp=TarSp2,gerProHp=TarProHp2},
                                    ?set_ger(Targer2),
                                    LinkAction = ?pro_action(?ACTION_NORMAL_HURT,E,[],HpDecf,ProHpDecf,TarAddSp,?STATE_DEFAULT),
                                    HurtAction = 
										case TarGerHp2 =< 0 of
											true ->
												{IsReborn,RebornAction2,SrcKillHooks} = do_reborn(AtkPos,E,Targer2,{0,[],[]}),
												case IsReborn of
													false ->
														StandFellowAction = stand_fellow(E),
														StandFellowAction++ SrcKillHooks ++ [?action(?ACTION_DEAD,E,[],0,0,?STATE_DEFAULT)] ;
													_ ->
														RebornAction2 
												end;
											_ ->
												[]
										end,
                                    {HurtAction++InvAction++[LinkAction|ActionAcc],Acc}
                            end
                        end,{[],AtkInfo},Link) .

%% 反击
strike_back(DataSkill, SrcGerPos, StrikeBack, #attack_info{type=Type}=Acc) ->
    #data_skill{attackActionID=AtkActionID,defendActionID=DefendActionID} = DataSkill,
    lists:foldl(fun({E, Damage, Ratio,ActionType,XGerTypeID}, #attack_info{action=ActionListT,append=Append,atk_skill=LastAtkSkill} = AccT) ->
                    case Damage =< 0 of
                        true ->
                            AccT;
                        _ ->
                            #ger{gerHp=GerHp,gerBase=#gerBase{gerTypeID=TGerTypeID}} = ?get_ger(E),
                            %% 黑龙的眩晕是在攻击结束后做的特殊处理,所以攻击时的是否反击判断对黑龙不起作用
                            case GerHp =< 0 orelse is_need_skip_op(E) orelse XGerTypeID /= TGerTypeID of
                                true ->
                                    AccT;
                                _ ->
                                    UniqueHook = ?get_unique_hook(SrcGerPos),
                                    #ger{gerSp=TarSp,gerHp=TarGerHp,gerAttr=#gerAttr{gerSpMax=GerSpMax},gerProHp=GerProHp} = Targer = ?get_ger(SrcGerPos),
                                    ActionID = 
                                        case Type of
                                            unique -> if ActionType == 2 -> 164;ActionType==3->195; true -> 119 end;
                                            _ -> if ActionType == 2 -> 163;ActionType==3->194; true -> 115 end
                                        end,
                                    %% 攻击动作
                                    AtkAction = [?action(AtkActionID,E,[SrcGerPos],0,0,?STATE_DEFAULT),?action(ActionID,E,[],0,0,?STATE_DEFAULT)],
        
                                    %% 受击动作
                                    #unique_buffer{use=UniqueEffect} =TarUniqueBuffer= ?get_unique_buffer(SrcGerPos),
                                    #unique_effect{invinciblep=Invinciblep,spRa=SpRa} = UniqueEffect,
                                    {HarmValue0,TarAddSp} = 
                                        case Invinciblep of
                                            true ->
                                                {0,0};
                                            _ ->
                                                {trunc(Damage * Ratio),trunc(real_add_sp(TarSp,?HURT_ADD_SP,GerSpMax)*SpRa/100)}
                                            end,
                                    TarSp2 = TarSp + TarAddSp,
									{Inv0,HarmValue} = check_d_minHp(SrcGerPos,HarmValue0),
									damage_stastic(E,HarmValue),
									InvAction = if Inv0 -> set_d_inv(); true -> [] end,
									{ProHpDecf, HpDecf} = decf_ger_hp(GerProHp, HarmValue),
                                    TarGerHp2 = add_hp(TarGerHp, ?a(Targer, gerHpMax), HpDecf),
                                    GerProHp2 = GerProHp + ProHpDecf,
                                    HitAction = InvAction ++ [?pro_action(DefendActionID,SrcGerPos,[],HpDecf,ProHpDecf,TarAddSp,0)|AtkAction],
                                    Targer2 = Targer#ger{gerHp=TarGerHp2,gerSp=TarSp2,gerProHp=GerProHp2},
                                    ?set_ger(Targer2),
        
                                    {State,HurtAction} = 
                                        case TarGerHp2 =< 0 of
											true ->
												{IsReborn,RebornAction2,SrcKillHooks} = do_reborn(E,SrcGerPos,Targer2,{0,[],[]}),
												case IsReborn of
													false ->
														StandFellowAction = stand_fellow(SrcGerPos),
														{dead,StandFellowAction++SrcKillHooks ++ [?action(?ACTION_DEAD,SrcGerPos,[],0,0,?STATE_DEFAULT)]};
													0 ->
														{dead,SrcKillHooks ++ [?action(?ACTION_DEAD,SrcGerPos,[],0,0,?STATE_DEFAULT)]};
													_ ->
														{true,RebornAction2}
												end;
											_ ->
												{true,[]}
										end,
									#attack_info{action=AccT2Action}=AccT2 = do_damage_link(E,SrcGerPos,AccT#attack_info{action=HurtAction ++ HitAction ++ ActionListT,damage=HarmValue}),
									case State of
										dead ->
											AccT2#attack_info{append=Append};
										_ ->
											%% 处理沉睡变身
											#unique_effect{morph_status=MorphStatus, morph_info=MorphInfo} = UniqueEffect,
											AccT3 = 
												case MorphStatus =:= ?ger_morph_status_sleep of
													true ->
														{RatioM, AtkSkillM} = ?get_morph_info(?ger_morph_status_sleep, MorphInfo),
														MorphInfo2 = ?clear_morph_info(?ger_morph_status_sleep, MorphInfo),
														rm_old_target_maimed_info(SrcGerPos, SrcGerPos, ger_morph_status_sleep),
														MorphActions = [?action(146,SrcGerPos,[],0,0,?STATE_TALENT),?action(148,SrcGerPos,[],0,0,?STATE_TALENT)],
														?set_unique_buffer(SrcGerPos, TarUniqueBuffer#unique_buffer{use=UniqueEffect#unique_effect{morph_status=?ger_morph_status_normal, morph_info=MorphInfo2}}),
														#ger{gerBase=#gerBase{gerTypeID=BGerTypeID}}=?get_ger(SrcGerPos),
														AccT2#attack_info{action=MorphActions ++ AccT2Action, mstrike_back=[{SrcGerPos, RatioM, AtkSkillM,BGerTypeID}],atk_skill=LastAtkSkill};
													_ ->
														AccT2#attack_info{action = AccT2Action,atk_skill=LastAtkSkill}
												end,
											AccT4 = after_unique_hit_hook(UniqueHook,SrcGerPos,E,AccT3#attack_info{append=Append}),
											check_strike_back(DataSkill,E,AccT4)
									end
							end
					end
				end, Acc#attack_info{nstrike_back=[],ustrike_back=[]}, StrikeBack).

%% 反击
morph_strike_back(SrcGerPos, StrikeBack, Acc) ->
    lists:foldl(fun({E, Ratio, AtkSkill,XGerTypeID}, #attack_info{action=ActionListT,append=Append,atk_skill=LastAtkSkill} = AccT) ->
                    #ger{gerHp=GerHp,gerBase=GerBase=#gerBase{gerTypeID=TGerTypeID},gerAttr=ESrcAttr}=EGer = ?get_ger(E),
                    %% 黑龙的眩晕是在攻击结束后做的特殊处理,所以攻击时的是否反击判断对黑龙不起作用
                    case GerHp =< 0 orelse is_need_skip_op(E) orelse XGerTypeID /= TGerTypeID of
                        true ->
                            AccT;
                        _ ->
							                    %% 注意这个Damage算出来可能是0,这儿和果然的流程不一样了,反击伤害为0流程也要走
                    		{IsInv,Damage} = calc_skill_damage(E, SrcGerPos, Ratio, AtkSkill),
                            #data_skill{attackActionID=AtkActionID,defendActionID=DefendActionID} = AtkSkill,
                            UniqueHook = ?get_unique_hook(SrcGerPos),
							#ger{gerSp=TarSp,gerHp=TarGerHp,gerAttr=#gerAttr{gerSpMax=GerSpMax},gerProHp=GerProHp} = Targer = ?get_ger(SrcGerPos),
							%% 攻击动作
							TarUniqueHook = ?get_unique_hook(E),
							AtkAction = 
								case TarUniqueHook#unique_trigger.t12 of [] ->
																			 [?action(AtkActionID,E,[SrcGerPos],0,0,?STATE_DEFAULT),?action(149,E,[],0,0,?STATE_DEFAULT)];
									_ ->
										[?action(AtkActionID,E,[SrcGerPos],0,0,?STATE_DEFAULT),?action(182,E,[],0,0,?STATE_DEFAULT)]
								end,
							#unique_buffer{use=TarUniqueEffect} = TarUniqueBuffer = ?get_unique_buffer(SrcGerPos),
							%% 受击动作
							if IsInv -> AbsorbAction=InvAction=[],AbsortState=State=?STATE_DEFAULT
										,TarAddSp=HpDecf=HarmValue=ProHpDecf=0,Targer2=Targer,TarGerHp2=TarGerHp;
							   true ->
								   #unique_buffer{use=SrcUniqueEffect} = ?get_unique_buffer(E),
								   #unique_effect{morph_sleep_critic=MSC,cure_sub=CurSub,cure_add=CurAdd,spRa=SpRa} = SrcUniqueEffect,
								   {HarmValue0,TarAddSp,State} = 
									   case MSC of
										   0 ->
											   {Damage,trunc(real_add_sp(TarSp,?HURT_ADD_SP,GerSpMax)*SpRa/100),?STATE_DEFAULT};
										   _ ->
											   {Damage * MSC,trunc(real_add_sp(TarSp,?HURT_ADD_SP,GerSpMax)*SpRa/100),?STATE_CRIT} 
									   end,
								   TarSp2 = TarSp + TarAddSp,
								   {Inv0,HarmValue} = check_d_minHp(SrcGerPos,HarmValue0),
								   InvAction = if Inv0 -> set_d_inv(); true -> [] end,
								   damage_stastic(E,HarmValue),
								   {ProHpDecf, HpDecf} = decf_ger_hp(GerProHp, HarmValue),
								   TarGerHp2 = add_hp(TarGerHp, ?a(Targer, gerHpMax), HpDecf),
								   GerProHp2 = GerProHp + ProHpDecf,
								   %% 吸血
								   Absorb = ESrcAttr#gerAttr.gerAbsorb,
								   if Absorb =< 0 ->
										  AbsortState = ?STATE_DEFAULT,
										  AbsorbAction = [];
									  true ->
										  AbsortState = ?STATE_ABSORB,
										  AbsorbHp = erlang:max(1,trunc(HarmValue * Absorb /10000)),
										  CurAdd2 = calc_tr_ctl_add(CurAdd,GerBase#gerBase.gerTypeID,E),
										  AbsorbT2 = erlang:trunc(AbsorbHp * (100 - CurSub + CurAdd2/100) / 100),
										  AbsorbAction = [?action(?ACTION_ABSORB,E,[],AbsorbT2,0,?STATE_DEFAULT)],
										  %% 计算新的SrcGer
										  ESrcHp1 = add_hp(GerHp,?a(EGer,gerHpMax),AbsorbT2),
										  ESrcGer2 = EGer#ger{gerHp=ESrcHp1},
										  ?set_ger(ESrcGer2)								   
								   end,
								   Targer2 = Targer#ger{gerHp=TarGerHp2,gerSp=TarSp2,gerProHp=GerProHp2}
							end,
							HitAction = AbsorbAction ++ InvAction ++ [?pro_action(DefendActionID,SrcGerPos,[],HpDecf,ProHpDecf,TarAddSp,State bor AbsortState)|AtkAction],
                            ?set_ger(Targer2),
                            {GerState,HurtAction} = 
                                case TarGerHp2 =< 0 of
									true ->
										{IsReborn,RebornAction2,SrcKillHooks} = do_reborn(E,SrcGerPos,Targer2,{0,[],[]}),
										case IsReborn of
											false ->
												StandFellowAction = stand_fellow(SrcGerPos),
												{dead,StandFellowAction++SrcKillHooks ++ [?action(?ACTION_DEAD,SrcGerPos,[],0,0,?STATE_DEFAULT)]};
											0 ->
												{dead,SrcKillHooks ++ [?action(?ACTION_DEAD,SrcGerPos,[],0,0,?STATE_DEFAULT)]};
											_ ->
												{true,RebornAction2}
										end;
									_ ->
										{true,[]}
                                end,
                            #attack_info{action=AccT2Action} = AccT2_0 = do_damage_link(E,SrcGerPos,AccT#attack_info{action=HurtAction ++ HitAction ++ ActionListT,damage=HarmValue}),
							#attack_info{uaction=AStrikeBackUAction} = AccT2 =  after_strike_back(TarUniqueHook,E,SrcGerPos,AccT2_0#attack_info{ atk_skill=AtkSkill}),
                            case GerState of
                                dead ->
                                    AccT2#attack_info{append=Append,action = AStrikeBackUAction++ AccT2Action,atk_skill=LastAtkSkill};
                                _ ->
                                    %% 处理沉睡变身
                                    #unique_effect{morph_status=MorphStatus, morph_info=MorphInfo} = TarUniqueEffect,
                                    AccT3 = 
                                        case MorphStatus =:= ?ger_morph_status_sleep of
                                            true ->
                                                {Ratio, AtkSkill} = ?get_morph_info(?ger_morph_status_sleep, MorphInfo),
                                                MorphInfo2 = ?clear_morph_info(?ger_morph_status_sleep, MorphInfo),
                                                rm_old_target_maimed_info(SrcGerPos, SrcGerPos, ger_morph_status_sleep),
                                                MorphActions = [?action(146,SrcGerPos,[],0,0,?STATE_TALENT),?action(148,SrcGerPos,[],0,0,?STATE_TALENT)],
                                                ?set_unique_buffer(SrcGerPos, TarUniqueBuffer#unique_buffer{use=TarUniqueEffect#unique_effect{morph_status=?ger_morph_status_normal, morph_info=MorphInfo2}}),
												#ger{gerBase=#gerBase{gerTypeID=BGerTypeID}} = ?get_ger(SrcGerPos),
                                                AccT2#attack_info{action=AStrikeBackUAction++MorphActions ++ AccT2Action, mstrike_back=[{SrcGerPos, Ratio, AtkSkill,BGerTypeID}],atk_skill=LastAtkSkill};
                                            _ ->
                                                AccT2#attack_info{action = AStrikeBackUAction++ AccT2Action,atk_skill=LastAtkSkill}
                                        end,
                                    AccT4 = after_unique_hit_hook(UniqueHook,SrcGerPos,E,AccT3),
                                    check_strike_back(AtkSkill,E,AccT4)
                            end
                    end
                end, Acc#attack_info{mstrike_back=[]}, StrikeBack).

%% 判断是否还活着
is_dead(Pos) ->
    #ger{gerHp=GerHp} = ?get_ger(Pos),
    GerHp =< 0.
   
%% 获得精灵和他们的链接精灵列表
get_all_link(GerList) ->
    lists:foldl(fun(E, Acc) ->
                    case ?get_damage_link(E) of
                        {[],[],[]} ->
                            Acc;
                        {LinkA,LinkB,LinkC} ->
                            lists:foldl(fun(El, Acc2) ->
                                            case lists:member(El, Acc2) of
                                                true ->
                                                    Acc2;
                                                _ ->
                                                    [El|Acc2]
                                            end
                                        end, Acc, LinkA ++ LinkB++LinkC)
                    end
                end, GerList, GerList).

%% 判断是否进行反击
check_strike_back(DataSkill,AtkPos,#attack_info{type=Type,nstrike_back=NStrikeBack,ustrike_back=UStrikeBack,mstrike_back=MStrikeBack}=AttackInfo) ->
    StrikeBack = 
        case Type of
            unique -> UStrikeBack;
			unique_noSp_4 -> UStrikeBack;
			unique_noSp_3 -> UStrikeBack;
			unique_noSp_2 -> UStrikeBack;
			unique_noSp -> UStrikeBack;
			unique_critic -> UStrikeBack;
            _ -> NStrikeBack
        end,

    %% 反击, 进行反击前应该清除 nstrike_back ustrike_back mstrike_back, 带入数据进去会影响新的递归流程,导致表现错乱
    AttackInfo2 =
        case StrikeBack of
            [] ->
                AttackInfo;
            _ ->
                strike_back(DataSkill,AtkPos,StrikeBack,AttackInfo#attack_info{nstrike_back=[],ustrike_back=[],mstrike_back=[]}) 
        end,
    %% 沉睡反击
    case MStrikeBack of
        [] ->
            AttackInfo2;
        _ ->
            morph_strike_back(AtkPos, MStrikeBack, AttackInfo2#attack_info{nstrike_back=[],ustrike_back=[],mstrike_back=[]})
    end.

%% 添加dot buffer
add_dot_buffer(TargetList,#attack_info{damage=Damage}=AtkInfo,Duration,[ActionType, Ratio],SrcGerPos) ->
    lists:foldl(fun(E,#attack_info{uaction=Uaction}=AtkAcc) ->
                    %%dot在每回合开始前,而绝技的检测是在攻击后,所以会导致多一次扣血.这里只能做特殊处理
                    UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(E),
                    #unique_effect{dot=Dot,invinciblep=Invinciblep} = UseEffect,
                    case Invinciblep of
                        true ->
                            AtkAcc;
                        _ ->
                            DotDamage = erlang:trunc((Damage * Ratio / 100) / Duration),
							case ActionType of 1 -> Uaction2 = [?action(114,E,[],0,0,?STATE_TALENT)|Uaction];
								2 ->Uaction2 = [?action(190,E,[],0,0,?STATE_TALENT)|Uaction];
								_ ->?ERR("bad type:~w",[[Duration,ActionType,Ratio,SrcGerPos,?get_ger(SrcGerPos)]]),
									Uaction2 = [?action(190,E,[],0,0,?STATE_TALENT)|Uaction]
							end,
                            UseEffect2 = UseEffect#unique_effect{dot=[{SrcGerPos,DotDamage,Duration,ActionType}|Dot]} ,
                            ?set_unique_buffer(E,UniqueBuffer#unique_buffer{use=UseEffect2}),
                            AtkAcc#attack_info{uaction=Uaction2}
                    end
                end,AtkInfo,TargetList).

%% dec_sp(ApplyList,Acc,Args,_SelectPos)->
%%     Dec = hd(Args),
%%     lists:foldl(fun(E, #attack_info{uaction=Uaction}=AtkAcc)->
%%                         #ger{gerSp=Sp} = Ger = ?get_ger(E),
%%                         NewSp = erlang:max(Sp - Dec, 0),
%%                         IDec = Sp - NewSp,
%%                          Uaction2 = [?action(103,E,[],0,IDec,?STATE_TALENT)|Uaction],
%%                         ?set_ger(Ger#ger{gerSp=NewSp}),
%%                         AtkAcc#attack_info{uaction=Uaction2}
%%                         end, Acc, ApplyList).

damage_double(SrcPos,#attack_info{uaction=Uaction}=Acc,_Args)->
    UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(SrcPos),
    #unique_effect{damage_double=DD} = UseEffect,
    UseEffect2 = UseEffect#unique_effect{damage_double=DD+1},
    ?set_unique_buffer(SrcPos,UniqueBuffer#unique_buffer{use=UseEffect2}),
    Acc#attack_info{uaction=[?action(154,SrcPos,[],0,0,?STATE_DEFAULT)|Uaction]}.
    
add_trSkill_dot_buffer(TargetList,Damage,Duration,ActionID,ActionID2,SkinBuff) ->
	Damage1 = erlang:trunc(Damage*(1+SkinBuff#skin_buff.even_trainer_demage_plus/10000)),
	lists:foldl(fun(Pos,{ActionAcc1,ActionAcc2}) ->
						UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
						#unique_effect{dot=Dot,invinciblep=Invinciblep} = UseEffect,
						case Invinciblep of
							true ->
								{ActionAcc1,ActionAcc2};
							_ ->
								DotDamage1 = erlang:trunc(Damage1  / Duration),
								SkinBuff2 = ?get_pos_skinbuff(Pos),
								DotDamage = erlang:trunc(DotDamage1*(1-SkinBuff2#skin_buff.ger_demage_sub/10000)),
								UseEffect2 = UseEffect#unique_effect{dot=[{0,DotDamage,Duration,0}|Dot]} ,
								?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UseEffect2}),
								{[?action(ActionID,Pos,[],0,0,?STATE_DEFAULT)|ActionAcc1]
								,[?action(ActionID2,Pos,[],0,0,?STATE_DEFAULT)|ActionAcc2]}
						end
				end, {[],[]}, TargetList).

%% 清除一个精灵的buffer
clear_buffer(Pos) ->
    %% 残废判断
    Maimeds = ?get_ger_maimed(Pos),
    #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(Pos),
    #unique_effect{sleep=UESleep,weak=Weak,weakest=Weakest,confuse_inc=ConfuseInc,morph_sleep_critic=MSC,damage_double=DamageDouble,morph_has_status=MHS
				  ,dead_mark=DeadMark,holy_kill=HolyKill,target_add=TargetAdd,morph_status=MorphStatus, morph_info=MorphInfo ,con_attack=ConAttack
				  ,no_injury=NoInjury,hurt_cure=HurtCure,maxCount=MaxCC} = UniqueEffect,
    %% 重置
    ?set_ger_maimed(Pos, []),
	lists:foreach(fun(E) ->
						  EMaimed = ?get_maimed(E),
						  case EMaimed of 
							  #maimed{srcpos=SrcPos, type=Type, tarpos=TarPos} ->
								  rm_old_target_maimed_info(SrcPos, TarPos, Type);
							  _ -> ignore
						  end
				  end, Maimeds), 
	MorphStatus2 = case MorphStatus of 3 -> 2; _ -> MorphStatus end,
    UniqueEffect2 = #unique_effect{sleep=UESleep,weak=Weak,weakest=Weakest,confuse_inc=ConfuseInc,morph_sleep_critic=MSC,damage_double=DamageDouble,morph_has_status=MHS
								  ,dead_mark=DeadMark,holy_kill=HolyKill,target_add=TargetAdd,morph_status=MorphStatus2, morph_info=MorphInfo,con_attack=ConAttack
								  ,no_injury=NoInjury,hurt_cure=HurtCure,maxCount=MaxCC}, %% 这几个值需要保留
    ?set_unique_buffer(Pos,#unique_buffer{use=UniqueEffect2}),
    %%伤害链接判断
    Link = ?get_damage_link(Pos),
    case Link of
        {[],[],[]} ->
            [];
        {LinkA,LinkB,LinkC} ->
            lists:foreach(fun(E) ->
                            {TLink1,TLink2,TLink3} = ?get_damage_link(E),
                            ?set_damage_link(E, {lists:delete(Pos,TLink1),lists:delete(Pos,TLink2),lists:delete(Pos, TLink3)})
                        end,LinkA++LinkB++LinkC),
            %%重置
            ?set_damage_link(Pos, {[],[],[]}),
            ActionA = case LinkA of [] -> []; _ ->[?action(?ACTION_LINK_RMV,Pos,[],0,0,?STATE_TALENT)] end,
			ActionB = case LinkB of [] -> []; _ ->[?action(186,Pos,[],0,0,?STATE_TALENT)] end,
            ActionC = case LinkC of [] -> []; _ ->[?action(186,Pos,[],0,0,?STATE_TALENT)] end,
			ActionA ++ ActionB++ActionC
    end.

%%通过getTypeID判断当前战斗的类型，在挑战世界boss时，需要对战斗进行特殊处理,由于世界boss在两个配置中一致，为减少每次战斗都进行计算，故直接从配置读取
is_Boss_fight_type(GerTypeID)->
    NanmBossList = data_nanm:get(boss_id_list),
    lists:member(GerTypeID,NanmBossList).

%% 减去精灵血量
decf_ger_hp(_,0) ->
	{0,0};

%% decf_ger_hp(ProHp,D) when D < 0 ->
%% 	decf_ger_hp(ProHp,erlang:abs(D));
decf_ger_hp(ProHp,Damage0) ->
	Damage = erlang:abs(Damage0),
    if 
        ProHp >= Damage ->
           {-Damage,0};
        ProHp > 0 ->
            {-ProHp,ProHp - Damage};
        true ->
            {0,-Damage}
    end.

%%返回精灵觉醒第三阶段加成属性
%%return #add_attr{}
get_awake_third_step_add_attr(GerAwakeInfo,CrystalInfo)->
	Mul = role_crystal:calculate_crystal_awake_add(CrystalInfo,3),
    case lists:keyfind(3,#awake.step,GerAwakeInfo) of
        false->
            #add_attr{};
        #awake{skillID = SkillID}->
            case data_awake_skill:get({data_awake_skill,SkillID}) of
                ?undefined->
                    ?ERR("精灵觉醒的第三阶段技能在技能表中找不到 SkillID：~w ~n",[SkillID]),
                    #add_attr{};
                #add_attr{}=FindOne ->
                    ger_attr:append_add_attr_with_int(FindOne,Mul);
                _ ->
                    #add_attr{}
            end
    end.

get_diamond_ger_skillID(DiamondID,GerTypeID) ->
	case data_diamond_skill:get({DiamondID,GerTypeID}) of
		?undefined -> data_diamond_skill:get({DiamondID,0});
		ID -> ID
	end.

add_gerDiamond_trigger(UniqueHook, Ger) when is_record(UniqueHook, unique_trigger) ->
	{GerDiamondInfo,Pos,GerTypeID} =
		if is_record(Ger,ger) ->{Ger#ger.gerBase#gerBase.gerHolyGrailInfo,Ger#ger.gerBase#gerBase.gerPos,Ger#ger.gerBase#gerBase.gerTypeID};
		   is_record(Ger,gerSimple) -> {Ger#gerSimple.gerHolyGrailInfo,Ger#gerSimple.gerPos,Ger#gerSimple.gerTypeID};
		   true -> {[],1,0}
		end,
	case GerDiamondInfo of [] -> UniqueHook;
		_ ->
			DiamondInfo = GerDiamondInfo#holyGrail.diamondInfo,
			lists:foldl(fun({_,_,DiamondID},UniqueHookAcc) ->
								SkillID = get_diamond_ger_skillID(DiamondID,GerTypeID),
								case data_unique_effect:get(SkillID) of
									?undefined -> UniqueHookAcc;
									#data_unique_effect{opportune=Opportune,condition=Condition,apply_type=ApplyType,args=Args,replaceID=ReplaceID} ->
										#unique_trigger{t2=T2,t4=T4} = UniqueHookAcc,
										case Opportune of
											8 ->
												UniqueHookAcc;
											2 ->
												TPos = Condition + 2,
												Older = lists:delete(ReplaceID,erlang:element(TPos, T2)),
												UniqueHookAcc#unique_trigger{t2=erlang:setelement(TPos,T2,[SkillID|Older])};
											4 ->
												TPos = Condition + 2,
												Older = erlang:element(TPos, T4),
												UniqueHookAcc#unique_trigger{t4=erlang:setelement(TPos,T4,[SkillID|Older])};
											99 ->
												#unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
												TargetAdd = UniqueEffect#unique_effect.target_add,
												SkillRaUp = UniqueEffect#unique_effect.skill_ra_up,
												HC = UniqueEffect#unique_effect.hurt_cure,
												UniqueEffect2 = 
													case ApplyType of
														17 -> UniqueEffect#unique_effect{sleep=erlang:hd(Args)};
														19 -> UniqueEffect#unique_effect{weak=Args};
														20 -> UniqueEffect#unique_effect{weakest=erlang:hd(Args)};
														40 -> [Ty,Ra]=Args,UniqueEffect#unique_effect{holy_kill={Ty,Ra}};
														66 -> UniqueEffect#unique_effect{no_injury=erlang:hd(Args)};
														67 -> UniqueEffect#unique_effect{hurt_cure=[{1,erlang:hd(Args)}|HC]};
														68 -> UniqueEffect#unique_effect{hurt_cure=[{2,erlang:hd(Args)}|HC]};
														69 -> UniqueEffect#unique_effect{target_add=[{2,erlang:hd(Args)}|TargetAdd]};
														70 -> UniqueEffect#unique_effect{target_add=[{1,erlang:hd(Args)}|TargetAdd]};
														75 -> UniqueEffect#unique_effect{skill_ra_up=[erlang:hd(Args)|SkillRaUp]}
													end,
												?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2}),
												UniqueHookAcc;
											
											N ->
												TPos = N + 1,
												Older = erlang:element(TPos, UniqueHookAcc),
												erlang:setelement(TPos,UniqueHookAcc,[SkillID|Older])
										end
								end
						end,UniqueHook,DiamondInfo)
	end.

add_awake_five_step_trigger(UniqueHook,Ger,QualityBuffSkillIDList1) when is_record(UniqueHook,unique_trigger)->
    QualityBuffSkillIDList = [ID||ID<-QualityBuffSkillIDList1,lists:member(ID,data_ger_up_rank:get(ger_quality_weak_awake_skillIDList))],
    {GerAwakeInfo,Pos} = if
                        is_record(Ger,ger)->
                            {Ger#ger.gerBase#gerBase.gerAwakeInfo,Ger#ger.gerBase#gerBase.gerPos};
                        is_record(Ger,gerSimple)->
                            {Ger#gerSimple.gerAwakeInfo,Ger#gerSimple.gerPos};
                        true->
                            {[],1}
    end,
    TotalSkillIDList = case lists:keyfind(5,#awake.step,GerAwakeInfo) of
        false->
            QualityBuffSkillIDList;
        #awake{skillID=SkillID1}->
        	[SkillID1|QualityBuffSkillIDList]
    end,
    lists:foldl(fun(SkillID,Acc)->
        case data_unique_effect:get(SkillID) of
            undefined ->
                UniqueHook;
            #data_unique_effect{opportune=Opportune,condition=Condition,apply_type=ApplyType,args=Args,replaceID=ReplaceID} ->
                #unique_trigger{t2=T2,t4=T4} = Acc,
                case Opportune of
                    8 ->
                        Acc;
                    2 ->
                       	TPos = Condition + 2,
                       	Older = lists:delete(ReplaceID,erlang:element(TPos, T2)),
                       	Acc#unique_trigger{t2=erlang:setelement(TPos,T2,[SkillID|Older])};
                    4 ->
                       	TPos = Condition + 2,
                       	Older = erlang:element(TPos, T4),
                       	Acc#unique_trigger{t4=erlang:setelement(TPos,T4,[SkillID|Older])};
                   	99 ->
                        #unique_buffer{use=UniqueEffect} = UniqueBuffer =  ?get_unique_buffer(Pos),
                        UniqueEffect2 = 
                        case ApplyType of
                            17 ->
                                UniqueEffect#unique_effect{sleep=erlang:hd(Args)};
                            19 ->
                               	UniqueEffect#unique_effect{weak=Args};
                            20 ->
                                UniqueEffect#unique_effect{weakest=erlang:hd(Args)}
                        end,
                        ?set_unique_buffer(Pos,UniqueBuffer#unique_buffer{use=UniqueEffect2}),
                       	Acc;
                    N ->
                        TPos = N + 1,
                        Older = erlang:element(TPos, Acc),
                        erlang:setelement(TPos,Acc,[SkillID|Older])
                end
        end
    end,UniqueHook,TotalSkillIDList).

%%v4.0修改，增加品阶技能
%%生成在攻击完成之后附加的伤害（火系爆炸伤害）
get_awake_special_action(_SrcGerPos,_TarGerPos,Value,_OldHp,_Type) when Value =< 0 ->
	{0,[]};
get_awake_special_action(SrcGerPos,TarGerPos,Value,OldHp,Type)->
	case Type of
		normal->
			{0,[]};
		unique->
    		SrcGer = ?get_ger(SrcGerPos),
    		GerAwakeInfo = SrcGer#ger.gerBase#gerBase.gerAwakeInfo,
    		%%火系觉醒技能ID
    		FireFiveAwakeIDList = [ID||#awake{step=5,skillID=ID}<-GerAwakeInfo,lists:member(ID,data_awake:get(fire_type_skillID_list))],
    		%%品阶弱化炎爆技能ID
    		QualityFireIDList = case lists:keyfind(SrcGerPos,#fighter_info.pos,?get_alives()) of
    			false->
    				[];
    			#fighter_info{skill=#gerSkill{buff=BuffIDList}}->
    				[ID||ID<-BuffIDList,lists:member(ID,data_awake:get(fire_type_skillID_list))]
    		end,
    		get_awake_special_action2(SrcGerPos,TarGerPos,Value,OldHp,FireFiveAwakeIDList++QualityFireIDList);
        _ ->
        	{0,[]}
    end.
%%试用于火系炎爆技能，如果后面有其他技能，可以修改（品阶弱化炎爆和5觉炎爆，两者优先判断5觉炎爆，如果有一个触发，则后面的都不再触发）
get_awake_special_action2(_SrcGerPos,_TarGerPos,_Value,_OldHp,[])->
	{0,[]};
get_awake_special_action2(SrcGerPos,TarGerPos,Value,OldHp,[H|T])->
	case data_unique_effect:get(H) of
		?undefined->
			get_awake_special_action2(SrcGerPos,TarGerPos,Value,OldHp,T);
		#data_unique_effect{probability=Probability,args=Args}->
            case Probability >= random:uniform(100) of 
                false->
                	get_awake_special_action2(SrcGerPos,TarGerPos,Value,OldHp,T);
                _ ->
                    AddHp =  -erlang:trunc(Value*erlang:hd(Args)/100),
					State = 
						case OldHp+AddHp >0 of
							true->
								?STATE_DEFAULT;
							false->
								IsReborn = ?is_reborn(TarGerPos),
								if 	
									IsReborn ->
										?STATE_DEAD;
									true ->
										?STATE_DEFAULT
								end
						end,
					{AddHp,[?action(?ACTION_EXPLOSED_ID,TarGerPos,[],AddHp,0,State)]}
			end
	end.

update_target_maimed(SrcGerPos,TarGerPos,Type,Duration)->
    update_target_maimed(SrcGerPos,TarGerPos,Type,Duration, 0,false).

update_target_maimed(SrcGerPos,TarGerPos,Type,Duration,EffectID)->
	update_target_maimed(SrcGerPos,TarGerPos,Type,Duration,EffectID,false).

update_target_maimed(SrcGerPos,TarGerPos,Type,Duration,EffectID,NotForce)->
    #unique_buffer{use=UniqueBuffer} = ?get_unique_buffer(TarGerPos),
    #unique_effect{invinciblep=Invinciblep} = UniqueBuffer,
    case Invinciblep andalso not(NotForce) of 
        true ->
            invinciblep; %% NotForce = true, 无论是否无敌都添加
        _ ->
			clean_target_maimed(TarGerPos,Type),
            %% 先清除旧的,再添加新的
        	rm_old_target_maimed_info(SrcGerPos,TarGerPos,Type),
            ID = new_maimed_id(),
            NewCM = #maimed{id=ID,type=Type,srcpos=SrcGerPos,tarpos=TarGerPos,duration=Duration,effectId=EffectID},
            OldTargetMaimed2 = ?get_target_maimed(SrcGerPos),
            ?set_target_maimed(SrcGerPos, [ID|OldTargetMaimed2]),
            case is_maimed_type(Type) of
                true ->
                    ?add_ger_maimed(TarGerPos, ID);
                _ ->
                    ?add_ger_other_maimed(TarGerPos, ID) 
            end,
            ?set_maimed(ID, NewCM)  
    end.

clean_target_maimed(TarGerPos,Type)->
	lists:foreach(fun(E)->
								#maimed{type=MType,srcpos=MSrcPos} = ?get_maimed(E),
								if MType == Type ->
									   rm_old_target_maimed_info(MSrcPos,TarGerPos,Type);
								   true ->
									   ignore
								end
						end,?get_ger_maimed(TarGerPos)).

%% 清除src施加给tar的某类buff计数器.这里只能从src这边清除,因为tar只记录眩晕类的
%% buuf ID,而src记录的是所有需要计数的buff.之后tar记录的这些应该移入unique_buff
rm_old_target_maimed_info(SrcPos,TarPos,Type) ->
    OldTargetMaimeds = ?get_target_maimed(SrcPos),
    NewIDList = 
        lists:foldl(fun(E,Acc) ->
                        #maimed{id=ID,type=MType,tarpos=MTarPos} = ?get_maimed(E),
                        case MType =:= Type andalso MTarPos =:= TarPos of 
                            true ->
                                ?delete_ger_maimed(TarPos, ID),
                                ?delete_ger_other_maimed(TarPos, ID),
                                ?delete_maimed(ID),
                                Acc;
                            _ ->
                                [E|Acc] 
                        end
                    end,[],OldTargetMaimeds),
	?set_target_maimed(SrcPos,NewIDList).

get_display_id_and_state(?ACTION_FROZEN_ID)->     {?ACTION_UNFROZEN,?STATE_TALENT};
get_display_id_and_state(?ACTION_FROZEN_ID_1)->     {237,?STATE_DEFAULT};
get_display_id_and_state(?ACTION_PARALYSIS_ID)->  {?ACTION_UNPARALYSIS,?STATE_TALENT};
get_display_id_and_state(?ACTION_PARALYSIS_ID_1) -> {249, ?STATE_TALENT};
get_display_id_and_state(6)->    {?ACTION_UNREEL,?STATE_DEFAULT};
get_display_id_and_state(232)->    {233,?STATE_DEFAULT};
get_display_id_and_state(240)->    {241,?STATE_DEFAULT};
get_display_id_and_state(?ACTION_UNSLEEP)->  {?ACTION_UNSLEEP,?STATE_DEFAULT};
get_display_id_and_state(?ACTION_LINK_RMV)-> {?ACTION_LINK_RMV,?STATE_DEFAULT};
get_display_id_and_state(186)->  {186,?STATE_DEFAULT};
get_display_id_and_state(174) -> {174,?STATE_DEFAULT};
get_display_id_and_state(224) -> {225, ?STATE_DEFAULT};
get_display_id_and_state(_)->    {?ACTION_UNFROZEN,?STATE_TALENT}.

%% 应策划要求,这里需要将103放在113的前面,且两边轮流方技能,
%% 其他的没做要求,这里直接将103放到最前面
talent_skill_sort({A,_,_,_}, {B,_,_,_}) ->
    #data_talent_trigger{actionID=ActionIDA} = data_talent_skill:get(A),
    #data_talent_trigger{actionID=ActionIDB} = data_talent_skill:get(B),
    if
        ActionIDA =:= 103 ->
            true;
        ActionIDB =:= 103 ->
            false;
        true ->
            ActionIDA > ActionIDB
    end.

%% 迷惑(随机x个精灵,从4个效果中随机出1个加上)
%% 效果:1 诱惑 2 暴怒 3 疲惫 4 脆弱
%% allure rage weary weak
%% TODO 这几个技能使用新的计数方式,而旧有的check_unique_duration不进行修改
%% 实际上maimed应该处理为一个通用的计数器,挂在施放技能的那个精灵上
%% unique_buffer去掉effects,不对技能进行计数,单纯反应一个精灵的buff,
%% 包括眩晕、冰冻、暴怒等各种属性,然后属性的有效期由施加方来确定
do_confuse(GerPos, ApplyListT, EffectID, Duration, [Base, ActionType,WeightList]
		  , #attack_info{uaction=ActionList,confuse_list=ConfuseList} = AttackInfo) ->
    #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(GerPos),
    #unique_effect{confuse_inc=ConfuseInc} = UniqueEffect,
    ApplyList = filter_invincible(ApplyListT),
    Num = Base + ConfuseInc,   
    {ApplyList1, _} = util:safe_split(Num, util:random_list(ApplyList)),
	ApplyList2 = lists:foldl(fun(E,EAcc) -> #ger{gerHp=EGerHp}=?get_ger(E),if EGerHp >0 -> [E|EAcc]; true -> EAcc end end, [], ApplyList1),
	case ApplyList2 of
		[] ->
			AttackInfo; 
		_ ->
			%% 释放迷惑的动作
			NAction = case ActionType of 1 -> [?action(134, GerPos, [], 0, 0, ?STATE_DEFAULT)|ActionList];
						  2 -> [?action(199, GerPos, [], 0, 0, ?STATE_DEFAULT)|ActionList];
						  3 ->case ConfuseList of 
								  [] -> [?action(242, GerPos, [], 0, 0, ?STATE_DEFAULT)|ActionList];
								  _ -> ActionList
							  end;
						  4 -> [?action(253, GerPos, [], 0, 0, ?STATE_DEFAULT)|ActionList]
					  end,
			ConfuseList2 = case ActionType of 3 -> ApplyList2++ConfuseList;_ -> ConfuseList end,
			lists:foldl(fun(E, Acc) -> 
								do_confuse_1(GerPos, E, EffectID, Duration, 1,util:random_one_from_weigh_list(WeightList), Acc)
						end, AttackInfo#attack_info{uaction=NAction,confuse_list=ConfuseList2}, ApplyList2)
	end.

do_confuse_1(SrcPos, TarPos, EffectID, Duration, ActionType,Type, #attack_info{uaction=ActionList}=AttackInfo) ->
    UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(TarPos),
    {UseEffect2,ActionID,HAction} = 
        case Type of
            %% 这里统一用maimed计数(其实现在原本就是设计为通用的计数器)计数(其实现在原本就是设计为通用的计数器)
            {allure, 0} ->
                {UseEffect, 127,[]};
			{allure, 1} ->
                {UseEffect, 244,[?action(243, TarPos, [], 0, 0, ?STATE_DEFAULT)]};
			{allure, 2} ->
                {UseEffect, 244,[?action(243, TarPos, [], 0, 0, ?STATE_DEFAULT)]};
            {rage, Inc} ->
				#unique_effect{atk_dmg_plus={DP,ADList,RList,ZList}} = UseEffect,
				{RList2,ZList2} = case ActionType of 1 -> {[SrcPos|lists:delete(SrcPos, RList)],ZList};
									  _ -> {RList,[SrcPos|lists:delete(SrcPos, ZList)]}
								  end,
                {UseEffect#unique_effect{in_rage=true,atk_dmg_plus={DP+Inc,ADList,RList2,ZList2}}, 128,[]};
            {weary, Inc} ->
                #unique_effect{atk_dmg_sub={ADS,AList,BList}} = UseEffect,
				case ActionType of
					1 -> 
						AList2 = [SrcPos|lists:delete(SrcPos, AList)],
						{UseEffect#unique_effect{atk_dmg_sub={ADS + Inc,AList2,BList}}, 129,[]};
					2 -> 
						BList2 = [SrcPos|lists:delete(SrcPos, BList)],
						{UseEffect#unique_effect{atk_dmg_sub={ADS + Inc,AList,BList2}}, 189,[]}
				end;
            {weak, Inc} ->
				#unique_effect{damage_plus={DP,AList,BList}} = UseEffect,
				case ActionType of
					1 -> 
						AList2 = [SrcPos|lists:delete(SrcPos, AList)],
						{UseEffect#unique_effect{damage_plus={DP + Inc,AList2,BList}}, 130,[]};
					2 -> 
						BList2 = [SrcPos|lists:delete(SrcPos, BList)],
						{UseEffect#unique_effect{damage_plus={DP + Inc,AList,BList2}}, 188,[]}
				end
		end,
	case Type of {allure,2} -> update_target_maimed(TarPos,TarPos,Type,Duration,EffectID);
		_ -> update_target_maimed(SrcPos,TarPos,Type,Duration,EffectID)
	end,
    TarAction = [?action(ActionID, TarPos, [], 0, 0, ?STATE_DEFAULT)|HAction],
    ?set_unique_buffer(TarPos, UniqueBuffer#unique_buffer{use=UseEffect2}),
    AttackInfo#attack_info{uaction=TarAction++ActionList}.      

do_confuse_0(_SrcPos,_TarPos,_EffectID,_Duration,_ActionType,[],AttackInfo) ->
	AttackInfo;
do_confuse_0(SrcPos, TarPos,EffectID,Duration,ActionType,[Type|OtherType],AttackInfo) ->
	AttackInfo2 = do_confuse_1(SrcPos,TarPos,EffectID,Duration,ActionType,Type,AttackInfo),
	do_confuse_0(SrcPos,TarPos,EffectID,Duration,ActionType,OtherType,AttackInfo2).

do_confuse_multi(GerPos, ApplyListT, EffectID, Duration, [ActionType,TypeList], AttackInfo) ->
    %#unique_buffer{use=UniqueEffect} = ?get_unique_buffer(GerPos),
    %#unique_effect{confuse_inc=ConfuseInc} = UniqueEffect,
    ApplyList2 = filter_invincible(ApplyListT),
    %{ApplyList2, _} = util:safe_split(ConfuseInc, util:random_list(ApplyList)),
    case ApplyList2 of
        [] ->
            AttackInfo; 
        _ ->
            lists:foldl(fun(E, Acc) -> do_confuse_0(GerPos, E, EffectID, Duration, ActionType,TypeList, Acc) end
					   , AttackInfo, ApplyList2)
    end.
    
new_maimed_id() ->
    case ?get_maimed_id() of
        undefined ->
            ?set_maimed_id(1),
            1;
        X ->
            NX = X + 1,
            ?set_maimed_id(NX),
            NX
    end.

is_sleep(GerPos) ->
    lists:any(fun(E) ->
                #maimed{type=Type} = ?get_maimed(E),
                Type =:= ?ACTION_UNSLEEP 
              end, ?get_ger_maimed(GerPos)).

wakeup(GerPos) ->
    Maimeds = ?get_ger_maimed(GerPos),
    Maimeds2 = 
        lists:foldl(fun(E, Acc) ->
                        #maimed{id=ID,type=Type,srcpos=SrcPos,effectId=EffectID} = ?get_maimed(E),
                        case Type =:= ?ACTION_UNSLEEP of
                            false ->
                                [E|Acc];
                            _ ->
                                ?delete_target_maimed(SrcPos, ID),
                                ?delete_maimed(ID),
								UniqueBuffer = #unique_buffer{effects=Effects} = ?get_unique_buffer(GerPos), 
                                Effects2 = [E0||#effect_record{effectId=EEFTID}=E0<-Effects,EEFTID /= EffectID],
                                ?set_unique_buffer(GerPos, UniqueBuffer#unique_buffer{effects=Effects2}),
                                Acc
                        end
                    end, [], Maimeds),
    ?set_ger_maimed(GerPos, Maimeds2).

%% 清除己方的减益,敌方的增益buff
camp_clear_buff(SelfPos, #attack_info{uaction=Uaction}=AtkInfo) ->
    Alives = ?get_alives(),
    SrcCamp = ?sign(SelfPos),
    {Selfs, Enemys} = 
        lists:foldl(fun(#fighter_info{pos=E}, {SAcc, EAcc}) when abs(E) < 10 ->
                        case ?sign(E) =:= SrcCamp of
                            true ->
                                {[E|SAcc], EAcc};
                            _ ->
                                {SAcc, [E|EAcc]}
                        end;
					   (_,D) ->
							D
                    end, {[],[]}, Alives),
    Uaction2 = camp_clear_buff_1(Selfs, false, 132, Uaction),
    Uaction3 = camp_clear_buff_1(Enemys, true, 131, Uaction2),
    AtkInfo#attack_info{uaction=Uaction3}.

unReel_ger(#attack_info{uaction=Uaction}=AtkInfo,GerPos) ->
    Uaction2 = camp_clear_buff_2(GerPos, [?action(278,GerPos,[],0,0,?STATE_TALENT)|Uaction], false),
    AtkInfo#attack_info{uaction=Uaction2}.

do_unique_flow(SrcGerPos,AtkInfo=#attack_info{uaction=Uaction,atk_skill=DataSkill,afteraction=LastAfterAction},Args) ->
	{TarCount,TarPosList} = lists:foldl(fun(E,{CountAcc,TarPosListAcc})-> 
												#unique_buffer{use=UniqueBuffer}=?get_unique_buffer(E),
												case UniqueBuffer#unique_effect.invinciblep of
													true -> {CountAcc,TarPosListAcc};
													_ -> {CountAcc+1, [E|TarPosListAcc]}
												end
										end,{0,[]},enemy_alive(SrcGerPos)),
	Uaction2 = [?pro_action(192,SrcGerPos,[],0,0,0,?STATE_DEFAULT),?pro_action(179,SrcGerPos,[],0,0,0,?STATE_DEFAULT)]++Uaction,
	if TarCount < 1 -> AtkInfo#attack_info{uaction=Uaction2};
	   true -> 
		   #ger{gerAttr=SrcAttr2}=SrcGer=?get_ger(SrcGerPos),
		   	#data_skill{damageRatio=DamageRatio1}=DataSkill,
		   #dynamic_buffer{attr=SrcDynamicBuffer} = erlang:get({dynamic_buffer,SrcGerPos}),
		   #unique_buffer{use=SrcUniqueBuffer} = ?get_unique_buffer(SrcGerPos),
		   #unique_effect{attack_add=SrcAttackAdd,atk_dmg_plus={ADP,_,_,_}} = SrcUniqueBuffer,
		   %%由于魔典、科技、附魔、觉醒的属性加成都已经加入到gerExtra中，所以此处直接使用gerExtra即可
		   SrcEnchantAdd = SrcGer#ger.gerExtra,
		   %根据技能类型为技能伤害加成添加上附魔加成
		   SrcGerSkinBuff = ?get_pos_skinbuff(SrcGerPos),
		   DamageRatio = erlang:max((DamageRatio1 + SrcEnchantAdd#add_attr.gerUskillDemageRate+SrcGerSkinBuff#skin_buff.ger_attack_plus),1),
		   %% 在这儿加上动态属性
		   SrcAttr = add_talent_to_gerAttr(SrcAttr2,SrcDynamicBuffer),
		   HarmValueT = 
			   erlang:max(1,erlang:trunc(SrcAttr#gerAttr.gerAttack * DamageRatio * 0.0001 * 
															 (1 + ADP  / 100 + (SrcEnchantAdd#add_attr.gerExtraDemageRate + SrcAttackAdd) / 10000) 
										)
						 ),
		   damage_stastic(SrcGerPos,HarmValueT),
		   HarmValue2 = trunc(HarmValueT * hd(Args) / TarCount),
		   {XDamageList,ActionList,RebornActions,SencondLinkAction1} = lists:foldl(fun(TarGerPos,{HDamageactionAcc,ActionListAcc,RebornActionAcc,SecondLinkActionAcc})->
											#ger{gerAttr=TarAttr,gerHp=TarHp,gerProHp=TarPropHp,gerSp=TarSp}=TarGer=?get_ger(TarGerPos),
											{Inv0,HarmValue} = check_d_minHp(TarGerPos,HarmValue2),
											InvAction = if Inv0 -> set_d_inv(); true -> [] end,
											{ProHpDecf,HpDecf} = decf_ger_hp(TarPropHp,HarmValue),
											TarHp2 = add_hp(TarHp,TarAttr#gerAttr.gerHpMax,HpDecf),
											TarProHp2 = TarPropHp + ProHpDecf,
											TarAddSp = real_add_sp(TarSp,?HURT_ADD_SP,(TarGer#ger.gerAttr)#gerAttr.gerSpMax),
											TarSp2 = TarSp + TarAddSp,
											TarGerT = TarGer#ger{gerHp=TarHp2,gerProHp=TarProHp2,gerSp=TarSp2},
											State = 
												case TarHp2 =< 0 of
													true ->
														{IsReborn,RebornAction,_} = do_reborn(0,TarGerPos,TarGerT,{0,[],[]}),
														case IsReborn of
															false ->
																?STATE_DEAD;
															_ ->
																?STATE_DEFAULT
														end;
													_ ->
														RebornAction = [],
														?STATE_DEFAULT
												end,
											erlang:put(TarGerPos,TarGerT),
											HDamageAction = [?pro_action(?ACTION_NORMAL_HURT,TarGerPos,[],HpDecf,ProHpDecf,TarAddSp,State)],
											%#attack_info{action=LinkAction}=do_damage_link(0,TarGerPos,#attack_info{damage=HarmValue}),
											{SecondLinkAction,_} = do_damage_link2(SrcGerPos,TarGerPos,#attack_info{damage=HarmValue}),									
											{[?pro_action(180,TarGerPos,[],0,0,0,?STATE_DEFAULT)|HDamageactionAcc]
											,InvAction++HDamageAction++ActionListAcc,RebornAction++RebornActionAcc,SecondLinkAction++SecondLinkActionAcc}
									end,{[],[],[],[]},TarPosList),
		   AtkInfo#attack_info{uaction=RebornActions++ActionList++XDamageList++Uaction2,afteraction=SencondLinkAction1++LastAfterAction}
	end.

camp_clear_buff_1(GerList, GainPT, ActionID, ActionList) ->
    lists:foldl(fun(GerPos, GActionAcc) ->
                    GUB = #unique_buffer{use=UniqueEffect, effects=Effects} = ?get_unique_buffer(GerPos),

                    %% 施放动画
                    GActionAcc2 = [?action(ActionID, GerPos, [], 0, 0, ?STATE_DEFAULT)|GActionAcc],

                    %% 清除unique_effect的动画
                    {UniqueEffect2, ActionList2, Effects2} = 
						lists:foldl(fun(#effect_record{trSkill=TrSkill}=EE,{UseEffectAcc,ActionAcc,EffectsAcc}) when TrSkill=/=0 ->
											{UseEffectAcc,ActionAcc,[EE|EffectsAcc]};
									   (#effect_record{effectId=EffectID,subtype=SubType,trSkill=TrSkill,duration=Duration} = EE, {UseEffectAcc, ActionAcc, EffectsAcc} = UAEAcc) ->
											case data_unique_effect:get(EffectID) of
												undefined ->
													UAEAcc;
												#data_unique_effect{gainp=GainP,apply_type=ApplyType} ->
													if ApplyType == 10 andalso Duration > 100 ->
														   UAEAcc;
													   true ->
														   case GainP =:= GainPT andalso TrSkill =:= 0 of
															   false -> 
																   {UseEffectAcc, ActionAcc, [EE|EffectsAcc]};
															   _ ->
																   {UseEffectAcc2, Action} = sub_unique_effect(GerPos, UseEffectAcc, EffectID, SubType,[]),
																   {UseEffectAcc2, Action ++ ActionAcc, EffectsAcc}
														   end 
													end
											end
									end, {UniqueEffect, GActionAcc2, []}, Effects),
					

                    %% dot 单独处理
                    {UniqueEffect3, ActionList3} = 
                        case GainPT of
                            false ->
                                %% 清除dot
                                #unique_effect{dot=Dot} = UniqueEffect2,
                                case Dot of
                                    [] ->
                                        {UniqueEffect2, ActionList2};
                                    _ ->
                                        {UniqueEffect2#unique_effect{dot=[]}, get_dot_rmv_action(Dot,GerPos)++ActionList2}
                                end;
                            _ ->
                                {UniqueEffect2, ActionList2}
                        end, 

                    ?set_unique_buffer(GerPos, GUB#unique_buffer{use=UniqueEffect3, effects=Effects2}),
                    camp_clear_buff_2(GerPos, ActionList3, GainPT) 
                end, ActionList, GerList).

get_dot_rmv_action(ActionList,GerPos) when is_list(ActionList) ->
	[get_dot_rmv_action(Type,GerPos)||{_,_,_,Type} <- ActionList];
get_dot_rmv_action(2,GerPos) ->
	?action(191,GerPos,[],0,0,?STATE_TALENT);
get_dot_rmv_action(_,GerPos) ->
	?action(?ACTION_DOT_RMV,GerPos,[],0,0,?STATE_TALENT).

%% 注意不能清除中立状态的buff
camp_clear_buff_2(Pos, ActionList, GainP) ->
    %% EffectID为0的目前来说都是眩晕,属于负面效果
    Maimeds = ?get_ger_maimed(Pos),
    {Maimeds2, ActionList2} = 
        lists:foldl(fun(ID, {MaimedsAcc, AccT}) ->
                        #maimed{type=Type,srcpos=SrcPos,effectId=EffectID} = ?get_maimed(ID),
                        %% 新的列表+消除动作
                        {IsDel, NewAccT} =
		                        case EffectID of
		                            0 ->
		                                case GainP of
		                                    false ->
		                                        {DisActionID, State} = get_display_id_and_state(Type),
		                                        Action = ?action(DisActionID,Pos,[],0,0,State),
		                                        {true, [Action|AccT]};
		                                    _ ->
		                                        {false, AccT}
		                                end;
		                            _ ->
			                            UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos), 
		                                #data_unique_effect{gainp=GainPT, apply_type=_ApplyType} = data_unique_effect:get(EffectID),
		                                case GainP =:= GainPT of
		                                    false ->
		                                        {false, AccT};
		                                    _ ->
		                                        {UseEffect2, Action} = sub_unique_effect(Pos,UseEffect,EffectID,Type,[SrcPos]),
		                                        ?set_unique_buffer(Pos, UniqueBuffer#unique_buffer{use=UseEffect2}),
		                                        {true, Action ++ AccT}
		                                end
                                end, 
                        case IsDel of
                            false ->
                                {[ID|MaimedsAcc], NewAccT};
                            _ ->
                                ?delete_maimed(ID),
                                ?delete_target_maimed(SrcPos, ID),
                                {MaimedsAcc, NewAccT}
                        end 
                    end, {[], ActionList}, Maimeds),

    ?set_ger_maimed(Pos, Maimeds2),

    %% 去掉我方其他用maimed计数的buff,伤害链接+狐狸相关
    OMaimeds = ?get_ger_other_maimed(Pos),

    {OMaimeds2, ActionList3} = 
        lists:foldl(fun(ID, {MaimedsAcc, AccT}) ->
                            #maimed{type=Type,srcpos=SrcPos,effectId=EffectID} = ?get_maimed(ID),
                            #data_unique_effect{gainp=GainPT} = data_unique_effect:get(EffectID),
                            case GainPT =:= GainP of
                                false ->
                                    {[ID|MaimedsAcc], AccT};
                                _ ->
                                    ?delete_maimed(ID),
                                    ?delete_target_maimed(SrcPos, ID),
                                    UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
                                    {UseEffect2, Action} = sub_unique_effect(Pos,UseEffect,EffectID,Type,[SrcPos]),
                                    ?set_unique_buffer(Pos, UniqueBuffer#unique_buffer{use=UseEffect2}),
                                    {MaimedsAcc, Action ++ AccT}
                            end
                    end, {[], ActionList2}, OMaimeds),
    ?set_ger_other_maimed(Pos, OMaimeds2),
    ActionList3.

%% 过滤掉无敌的 
filter_invincible(PosList) ->
    lists:filter(fun(E) ->
				    #unique_buffer{use=TarUniqueBuffer} = ?get_unique_buffer(E),
					#unique_effect{invinciblep=TarInvinciblep} = TarUniqueBuffer,
                    not TarInvinciblep 
                end, PosList).                 

get_skin(Pos)->
	?get_skin_info(if Pos > 0 -> 1 ; true -> -1 end).

get_skinbuff(Pos)->
	?get_skin_buff(if Pos > 0 -> 1 ; true -> -1 end).            
                    
%% 在fight_order中过滤掉某个精灵
filter_fight_order(Pos, PosList) ->
    lists:foldl(fun(TargetList, Acc) ->
                    TargetList2 = lists:delete(Pos, TargetList),
                    case TargetList2 of
                        [] ->
                            Acc;
                        Any ->
                            [Any | Acc]
                    end
                end, [], PosList).

set_cur_limit(CurLimit) ->
	put(?cur_limit, CurLimit).

calc_cur_value(Cur)->
	case get(?cur_limit) of
		100 ->
			Cur;
		X when is_integer(X) ->
			erlang:trunc((X * Cur) / 100);
		_ ->
			Cur
	end.

set_d_minHp(DefenderMinHp) ->
	put(?d_minHp, DefenderMinHp).

get_d_minHp() ->
	case get(?d_minHp) of
		X when is_integer(X) -> X;
		_ -> -1
	end.

check_d_minHp(SPos,Harm)->
	MinHp = get_d_minHp(),
	if SPos > 0 orelse MinHp =< 0 orelse Harm == 0 ->
		   {false,Harm};
	   true ->
		   NowHp = 
			   lists:foldl(fun(#fighter_info{pos=Pos},HpAcc) when abs(Pos) < 10 ->
								   if Pos > 0 ->
										  HpAcc;
									  true ->
										  #ger{gerHp=GerHp} = ?get_ger(Pos),
										  if GerHp < 0 -> HpAcc;
											 true -> HpAcc + GerHp
										  end
								   end;
							  (_,V) ->
								   V
						   end , 0, ?get_alives()),
		   HHarm = NowHp - MinHp,
		   if HHarm =< Harm ->
				  #ger{gerHp=HMaxHp} = ?get_ger(SPos),
				  if HMaxHp >= HHarm -> {true,HHarm};
					 true -> {false,HMaxHp}
				  end;
			  true -> {false,Harm}
		   end
	end.

set_d_inv()->
	lists:foldl(fun(#fighter_info{pos=Pos},ActionAcc) ->
						if Pos > 0 orelse Pos < -10 ->
							   ActionAcc;
						   true ->
							   #unique_buffer{use=TarUniqueBuffer,effects=Effects} = Buffer = ?get_unique_buffer(Pos),
							   Effects2 =  lists:keydelete(1008,#effect_record.effectId,Effects),
							   TarUniqueBuffer2 = TarUniqueBuffer#unique_effect{invinciblep=true},
							   ?set_unique_buffer(Pos,Buffer#unique_buffer{use=TarUniqueBuffer2,effects=[#effect_record{effectId=1008,duration=10000}|Effects2]}),
							   case TarUniqueBuffer#unique_effect.invinciblep of
								   true ->
									   ActionAcc;
								   _ ->	
									   [?action(111,Pos,[],0,0,?STATE_TALENT)|ActionAcc]
							   end
						end
				end , [], ?get_alives()).

%% 判断是不是眩晕类的技能
%% TODO 眩晕之类的状态应该统一放入unique_buff中去,unique_buff做成个buff记录,
%% 然后持续回合由施放着计算,以后有条件的话可以把之前旧的新宠的unique都应该下
is_maimed_type({allure, _}) ->
    true;
is_maimed_type(Type) ->
    %%眩晕 沉睡　冰冻 麻痹
   lists:member(Type, [6,?ACTION_UNSLEEP,?ACTION_FROZEN_ID,?ACTION_FROZEN_ID_1,?ACTION_PARALYSIS_ID,224,240,232,248]). 

is_need_skip_op(Pos) ->
    case ?is_maimed(Pos) of
        true ->
            true;
        _ ->
            #unique_buffer{use=UseEffect} = ?get_unique_buffer(Pos),
            UseEffect#unique_effect.morph_status =:= ?ger_morph_status_sleep
    end.

trigger_morph_sleep(GerPos, #attack_info{uaction=Uaction, atk_skill=AtkSkill} = AttackInfo, EffectID, Duration, Args) ->
    UniqueBuffer = #unique_buffer{use=UseEffect=#unique_effect{morph_info=MorphInfo}} = ?get_unique_buffer(GerPos),
    MorphInfo2 = ?clear_morph_info(?ger_morph_status_sleep, MorphInfo),
    ActionList = [?action(147,GerPos,[],0,0,?STATE_DEFAULT),?action(145,GerPos,[],0,0,?STATE_DEFAULT)],
    NewUseEffect = UseEffect#unique_effect{morph_status=?ger_morph_status_sleep, morph_info=[{?ger_morph_status_sleep, {erlang:hd(Args), AtkSkill}}|MorphInfo2]}, 
    ?set_unique_buffer(GerPos,UniqueBuffer#unique_buffer{use=NewUseEffect}),
    update_target_maimed(GerPos,GerPos,ger_morph_status_sleep,Duration,EffectID),
    AttackInfo#attack_info{uaction=ActionList ++ Uaction}.

do_morph_sing(GerPos,ApplyList,EffectID,Duration,[StatusList, ActionType,WeightList],PlayActionType,AttackInfo) ->
    #unique_buffer{use=UniqueEffect} = ?get_unique_buffer(GerPos),
    #unique_effect{morph_status=MorphStatus} = UniqueEffect,
    case lists:member(MorphStatus, StatusList) of
        false ->
            AttackInfo;
        _ ->
			case ActionType of 
				1 ->
					PlayActionID = case PlayActionType of 0 -> 160; _ -> 161 end,
					Type = util:random_one_from_weigh_list(WeightList),
					PlayAction = ?action(PlayActionID, GerPos, [], 0, 0, ?STATE_DEFAULT),
					#attack_info{uaction=Uaction} = AttackInfo2 = lists:foldl(fun(E, Acc) -> do_morph_sing_1(GerPos, E, EffectID, Duration, Type, Acc) end, AttackInfo, ApplyList),
					AttackInfo2#attack_info{uaction=[PlayAction|Uaction]};
				_ ->
					PlayActionID = case PlayActionType of 0 -> 183; _ -> 184 end,
					PlayAction = ?action(PlayActionID, GerPos, [], 0, 0, ?STATE_DEFAULT),
					#attack_info{uaction=Uaction} = AttackInfo2 = lists:foldl(fun(E, Acc) -> do_morph_sing_0(GerPos, E, EffectID, Duration, WeightList, Acc) end, AttackInfo, ApplyList),
					AttackInfo2#attack_info{uaction=[PlayAction|Uaction]}
			end
    end.

do_morph_sing_b(GerPos,ApplyList,EffectID,Duration,[_,_,WeightList],_PlayActionType,AttackInfo) ->
    Type = util:random_one_from_weigh_list(WeightList),
    PlayAction = ?action(160, GerPos, [], 0, 0, ?STATE_DEFAULT),
    #attack_info{uaction=Uaction} = AttackInfo2 = lists:foldl(fun(E, Acc) -> do_morph_sing_1(GerPos, E, EffectID, Duration, Type, Acc) end, AttackInfo, ApplyList),
    AttackInfo2#attack_info{uaction=[PlayAction|Uaction]}.

do_morph_sing_1(SrcPos, TarPos, EffectID, Duration, Type, #attack_info{uaction=ActionList}=AttackInfo) ->
    UniqueBuffer = #unique_buffer{use=UseEffect} = ?get_unique_buffer(TarPos),
    {UseEffect2, Action, NeedRecord} = 
        case Type of
            {damage_sub, DS} ->
                #unique_effect{damage_sub={ODS,SList}} = UseEffect,
				SList2 = [SrcPos|lists:delete(SrcPos, SList)],
                {UseEffect#unique_effect{damage_sub={ODS + DS,SList2}}, ?action(150, TarPos, [], 0, 0, ?STATE_DEFAULT), true};
            {inc_sp, Inc} ->
                #ger{gerSp=GerSp, gerAttr=GerAttr} = Ger = ?get_ger(TarPos),
                #unique_effect{spRa=SpRa} = UseEffect,
                RInc = trunc(real_add_sp(GerSp, Inc, GerAttr#gerAttr.gerSpMax) * SpRa/100),
                GerSp2 = Ger#ger.gerSp + RInc,
                ?set_ger(Ger#ger{gerSp=GerSp2}),
                {UseEffect, ?action(113, TarPos, [], RInc, 0, ?STATE_TALENT), false};
            {atk_dmg_plus, ADP} ->
                #unique_effect{atk_dmg_plus={OADP,ADList,RList,ZList}} = UseEffect,
				ADList2 = [SrcPos|lists:delete(SrcPos, ADList)],
                {UseEffect#unique_effect{atk_dmg_plus={OADP + ADP,ADList2,RList,ZList}}, ?action(152, TarPos, [], 0, 0, ?STATE_DEFAULT), true};
            {invincible, _} ->
				#unique_effect{inv_way={InvWay,IList}} = UseEffect,
				InvWay2 = [sing|lists:delete(sing, InvWay)],
				IList2 = [SrcPos|lists:delete(SrcPos, IList)],
                {UseEffect#unique_effect{invinciblep=true,inv_way={InvWay2,IList2}}, ?action(154, TarPos, [], 0, 0, ?STATE_DEFAULT), true};
            {hit_critic, _} ->
				#unique_effect{hit_critic={_,HList}} = UseEffect,
				HList2 = [SrcPos|lists:delete(SrcPos, HList)],
                {UseEffect#unique_effect{hit_critic={true,HList2}}, ?action(156, TarPos, [], 0, 0, ?STATE_DEFAULT), true} 
        end,
    case NeedRecord of
        true ->
            update_target_maimed(SrcPos,TarPos,Type,Duration,EffectID,true);
        _ ->
            ignore
    end,
    ?set_unique_buffer(TarPos, UniqueBuffer#unique_buffer{use=UseEffect2}),
    AttackInfo#attack_info{uaction=[Action|ActionList]}.

do_morph_sing_0(_,_,_,_,[],AttackInfo)-> AttackInfo;
do_morph_sing_0(SrcPos,TarPos,EffectID,Duration,[Type|OtherType],AttackInfo) ->
	AttackInfo2 = do_morph_sing_1(SrcPos,TarPos,EffectID,Duration,Type,AttackInfo),
	do_morph_sing_0(SrcPos,TarPos,EffectID,Duration,OtherType,AttackInfo2).

sub_morph_sing_effect(GerPos, UseEffect, SubType,SrcPos) ->
	case SubType of
		{damage_sub, DS} ->
			#unique_effect{damage_sub={ODS,SList}} = UseEffect,
			SList2 = lists:delete(SrcPos, SList),
			case SList2 of
				[] -> {UseEffect#unique_effect{damage_sub={ODS - DS,[]}}, [?action(151, GerPos, [], 0, 0, ?STATE_DEFAULT)]};
				_ -> {UseEffect#unique_effect{damage_sub={ODS - DS,SList2}}, []}
			end;
		{atk_dmg_plus, ADP} ->
			#unique_effect{atk_dmg_plus={OADP,ADList,RList,ZList}} = UseEffect,
			ADList2 = lists:delete(SrcPos, ADList),
			case ADList2 of [] -> {UseEffect#unique_effect{atk_dmg_plus={OADP - ADP,[],RList,ZList}}, [?action(153, GerPos, [], 0, 0, ?STATE_DEFAULT)]};
				_ -> {UseEffect#unique_effect{atk_dmg_plus={OADP - ADP,ADList2,RList,ZList}}, []}
			end;
		{invincible, _} ->
			#unique_effect{inv_way={InvWay,IList}} = UseEffect,
			InvWay2 = lists:delete(sing, InvWay),
			IList2 = lists:delete(SrcPos, IList),
			case InvWay2 of [] -> 
								case IList2 of
									[] -> {UseEffect#unique_effect{invinciblep=false,inv_way={[],[]}}, [?action(155, GerPos, [], 0, 0, ?STATE_DEFAULT)]};
									_ -> {UseEffect#unique_effect{invinciblep=false,inv_way={[],IList2}}, []}
								end;
				_ -> case IList2 of
						 [] -> {UseEffect#unique_effect{inv_way={InvWay2,[]}}, [?action(155, GerPos, [], 0, 0, ?STATE_DEFAULT)]};
						 _ ->{UseEffect#unique_effect{inv_way={InvWay2,IList2}}, []}
					 end
			end;
		{hit_critic, _} ->
			#unique_effect{hit_critic={HState,HList}} = UseEffect,
			HList2 = lists:delete(SrcPos, HList),
			case HList2 of
				[] ->{UseEffect#unique_effect{hit_critic={false,[]}}, [?action(157, GerPos, [], 0, 0, ?STATE_DEFAULT)]};
				_ -> {UseEffect#unique_effect{hit_critic={HState,HList2}}, []}
			end
	end.

ger_change_status(GerPos, SkillType) ->
	case SkillType of
		unique ->
			#unique_buffer{use=UniqueEffect} = UniqueBuffer = ?get_unique_buffer(GerPos),
			#unique_effect{morph_status=MorphStatus} = UniqueEffect,
			ger_change_status(MorphStatus, GerPos, UniqueBuffer, UniqueEffect);
		_ ->
			[]
	end.

ger_change_status(?ger_morph_status_normal, _, _, _) ->
    [];

ger_change_status(?ger_morph_status_sleep, _, _, _) ->
    [];

ger_change_status(?ger_morph_status_sing, GerPos, UniqueBuffer, #unique_effect{morph_has_status=MHS}=UniqueEffect) ->
    case lists:member(?ger_morph_status_dance, MHS) of
        false ->
            [];
        _ ->
            ?set_unique_buffer(GerPos, UniqueBuffer#unique_buffer{use=UniqueEffect#unique_effect{morph_status=?ger_morph_status_dance}}), 
            [?action(158,GerPos,[],0,0,?STATE_DEFAULT)] 
    end;

ger_change_status(?ger_morph_status_dance, GerPos, UniqueBuffer, UniqueEffect) ->
    ?set_unique_buffer(GerPos, UniqueBuffer#unique_buffer{use=UniqueEffect#unique_effect{morph_status=?ger_morph_status_sing}}), 
    [?action(159,GerPos,[],0,0,?STATE_DEFAULT)].

morph_sleep_wakeup(GerPos, UniqueEffect) ->
    #unique_effect{morph_info=MorphInfo} = UniqueEffect,
    MStrikeBack = ?get_morph_info(?ger_morph_status_sleep, MorphInfo),
    MorphInfo2 = ?clear_morph_info(?ger_morph_status_sleep, MorphInfo),
    UniqueEffect2 = UniqueEffect#unique_effect{morph_status=?ger_morph_status_normal, morph_info=MorphInfo2},
    {[?action(146,GerPos,[],0,0,?STATE_DEFAULT),?action(148,GerPos,[],0,0,?STATE_DEFAULT)], MStrikeBack, UniqueEffect2}.

freeze_ground(GerPos,ApplyList,Duration,[ActionType],AttackInfo) ->
    #unique_buffer{use=UniqueEffect}=UniqueBuffer = ?get_unique_buffer(GerPos),
	#unique_effect{ningdong={NingDong,BuffV},atk_dmg_plus={DamagePlus,ADList,RList,ZList}} = UniqueEffect,
	if NingDong == 3 ->
		   		   ActionID = case ActionType of 1 -> ?ACTION_FREEZE_1; _ -> ?ACTION_FREEZE_2 end,
		   Action = [?action(ActionID, GerPos, ApplyList, 0, 0, ?STATE_DEFAULT),?action(220,GerPos,[],0,0,?STATE_TALENT)],
		   #attack_info{uaction=Uaction} = AttackInfo,
		   PlayAction =   lists:foldl(fun(E,Acc) ->
											 %TarGer = ?get_ger(E),
											 case update_target_maimed(GerPos,E,224,Duration) of
												 invinciblep ->	 Acc;
												 _ -> [?action(224,E,[],0,0,?STATE_TALENT)|Acc]
											 end end,Action, ApplyList),
		   ?set_unique_buffer(GerPos,UniqueBuffer#unique_buffer{use=UniqueEffect#unique_effect{ningdong={0,BuffV},atk_dmg_plus={DamagePlus-3*BuffV,ADList,RList,ZList}}}),
		   AttackInfo#attack_info{uaction=PlayAction++Uaction};
	   true ->
		   AttackInfo
	end.

cp_fighter(GerPos,AttackInfo=#attack_info{uaction=UAction})->
	#unique_buffer{use=UniqueEffect}=UniqueBuffer = ?get_unique_buffer(GerPos),
	#unique_effect{is_cp=IsCp} = UniqueEffect,
	case IsCp of 
		0 ->
			X = util:random_one_from_list(enemy_alive(GerPos)),
			#unique_buffer{use=#unique_effect{is_cp=TCP}} = ?get_unique_buffer(X),
			#ger{gerBase=#gerBase{gerTypeID=TGerTypeID0}} = ?get_ger(X),
			TGerTypeID1 = case TCP of {false,CPTypeIDValue} -> CPTypeIDValue;_  -> TGerTypeID0 end,
			#data_ger{baseTypeID=TGerTypeID} = data_ger:get(TGerTypeID0),
			Action = [?action(218, X, [X], TGerTypeID1, 0, ?STATE_DEFAULT),?action(218, GerPos, [X], TGerTypeID, 0, ?STATE_DEFAULT),?action(217,GerPos,[],0,0,?STATE_TALENT)],
			case TCP of
				0 -> ?set_unique_buffer(GerPos,UniqueBuffer#unique_buffer{use=UniqueEffect#unique_effect{is_cp=0}});
				_ -> ?set_unique_buffer(GerPos,UniqueBuffer#unique_buffer{use=UniqueEffect#unique_effect{is_cp=TGerTypeID}})
			end,
			AttackInfo#attack_info{uaction=Action++UAction};
		_ -> AttackInfo
	end.

gen_cp_fighter(SrcGerPos,FighterBase) ->
	#unique_buffer{use=#unique_effect{is_cp=ICP}} = ?get_unique_buffer(SrcGerPos),
	case ICP of
		false -> FighterBase;
		0 -> FighterBase;
		{_,_} ->FighterBase;
		TGerTypeID ->
			Ger = ?get_ger(SrcGerPos),
			#gerBase{gerQuality=GerQuality} = Ger#ger.gerBase,
			Skills = ger_lib:get_ger_skills(TGerTypeID,GerQuality),
			UniqueHook1 = 
				case data_unique_append:get(TGerTypeID) of
					undefined ->
						#unique_trigger{};
					List ->
						get_unique_hook(List,SrcGerPos,GerQuality,#unique_trigger{}) 
				end,
			%%在此处向精灵的必杀技能触发特效中添加上精灵第5阶段觉醒的触发技能
			UniqueHook2 = add_awake_five_step_trigger(UniqueHook1,Ger,Skills#gerSkill.buff),
			UniqueHook = add_gerDiamond_trigger(UniqueHook2,Ger),
			?set_unique_hook(SrcGerPos, UniqueHook),
			Fighter2 = FighterBase#fighter_info{pos=SrcGerPos,skill=Skills},
			Queue2 = lists:keystore(SrcGerPos, #fighter_info.pos, ?get_alives(), Fighter2),
			
			?set_alives(Queue2),
			Queue3 = lists:keystore(SrcGerPos, #fighter_info.pos, ?get_queue(), Fighter2),
			?set_queue(sort_fighters(Queue3)),
			case ?get_god_skill(SrcGerPos) of
				{0,InitGer} -> record_ger_god_skill(SrcGerPos,InitGer);
				_ -> ignore
			end,
			#unique_buffer{use=Use}=UBF = ?get_unique_buffer(SrcGerPos),
			?set_unique_buffer(SrcGerPos,UBF#unique_buffer{use=Use#unique_effect{is_cp={false,TGerTypeID}}}),
			Fighter2
	end.

%% FIXME 这是一个简化版的attack2，和attack2相互关联
calc_skill_damage(SrcGerPos, TarGerPos, BaseRatio, DataSkill) ->
    #unique_buffer{use=TarUniqueBuffer} = ?get_unique_buffer(TarGerPos),
    #unique_effect{damage_plus={TarDamagePlus,_,_},damage_sub={TarDamageSub,_},invinciblep=TarInvinciblep
				  ,damage_add={TarDamageAdd,_},damage_dec=TarDamageDec} = TarUniqueBuffer,
    case TarInvinciblep of
        true ->
            {TarInvinciblep,0};
        _ ->
        	#data_skill{damageRatio=DamageRatio1,damageType=DamageType,skillType=SkillType}=DataSkill,
            #ger{gerBase=#gerBase{gerTypeID=SrcGerTypeID},gerAttr=SrcAttr2}=SrcGer=?get_ger(SrcGerPos),
            #ger{gerBase=#gerBase{gerTypeID=TarGerTypeID},gerAttr=#gerAttr{gerHpMax=TarGerHpMax}=TarAttr2,gerHp=TarHp}=TarGer=?get_ger(TarGerPos),
            #dynamic_buffer{attr=SrcDynamicBuffer} = erlang:get({dynamic_buffer,SrcGerPos}),
            #dynamic_buffer{attr=TarDynamicBuffer} = erlang:get({dynamic_buffer,TarGerPos}),
            #unique_buffer{use=SrcUniqueBuffer} = ?get_unique_buffer(SrcGerPos),
            #unique_effect{sleep=Sleep,weak=Weak,attack_add=SrcAttackAdd,crit_hit_add=CritHitAdd,atk_dmg_sub={ADS,_,_},atk_dmg_plus={ADP,_,_,_},morph_status=SrcMorphStatus} = SrcUniqueBuffer,
        
            %%由于魔典、科技、附魔、觉醒的属性加成都已经加入到gerExtra中，所以此处直接使用gerExtra即可
            SrcEnchantAdd = SrcGer#ger.gerExtra,
            TarEnchantAdd = TarGer#ger.gerExtra,
            %根据技能类型为技能伤害加成添加上附魔加成
            DamageRatio2 = 
                case SkillType of
                    normal->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					normal_2->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					normal_3->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
					normal_4->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
                    normal_5 ->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerGskillDemageRate;
                    unique->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerUskillDemageRate;
                    unique2 ->
                        DamageRatio1 + SrcEnchantAdd#add_attr.gerUskillDemageRate;
                    _ ->
                        DamageRatio1
                end,

            SrcGerSkinBuff = ?get_pos_skinbuff(SrcGerPos),
            TarGerSkinBuff = ?get_pos_skinbuff(TarGerPos),
            DamageRatio = erlang:max((DamageRatio2+SrcGerSkinBuff#skin_buff.ger_attack_plus-TarGerSkinBuff#skin_buff.ger_demage_sub),1),

            %% 在这儿加上动态属性
            SrcAttr = add_talent_to_gerAttr(SrcAttr2,SrcDynamicBuffer),
            TarAttr = add_talent_to_gerAttr(TarAttr2,TarDynamicBuffer),
        	
        	CritValueT = BaseRatio * (1 + CritHitAdd / 10000),
        
            %% 沉睡暴击
            CritValueT1 = 
                case Sleep of
                    0 ->
                        CritValueT;
                    _ ->
                        case is_sleep(TarGerPos) of
                            false ->
                                CritValueT;
                            _ ->
                                CritValueT * Sleep
                        end
                end,
        
            %% 虚弱暴击
            CritValue1 = 
                case (SkillType =:= unique orelse SkillType =:= unique2) andalso Weak =/= [] of
                    false ->
                        CritValueT1;
                    _ ->
                        [X,N] = Weak,
                        case TarHp / TarGerHpMax * 100 =< X of
                            true ->
                                CritValueT1 * N;
                            _ ->
                                CritValueT1
                        end
                end,
        
            %% 舞步状态暴击
            CritValue =
                case SrcMorphStatus =:= ?ger_morph_status_dance of
                    true ->
                        CritValue1 * 2;
                    _ ->
                        CritValue1
                end,
            
            %% 伤害数值随机区间
            {A,B} = data_common:get(DamageType),
            RandomValue3 = random_float(A,B),
            %% 抗性系数
            Defence2 = 
                case erlang:atom_to_list(DamageType) of
                    [$p|_] ->
                        ?physic_defence;
                    _ ->
                        ?magic_defence 
                end,
                
                    
        	TarDamageAdd2 = calc_tr_ctl_add(TarDamageAdd,SrcGerTypeID,SrcGerPos),
            TarDamageDec2 = calc_tr_ctl_add(TarDamageDec,TarGerTypeID,TarGerPos),
        			
            %% 伤害计算公式
        	HarmValueT = RandomValue3 * CritValue * SrcAttr#gerAttr.gerAttack * Defence2 * DamageRatio * 0.0001,
        	{TarInvinciblep,erlang:max(1, erlang:trunc(HarmValueT * (1 + (ADP + TarDamagePlus - TarDamageSub - ADS) / 100 
                        + (SrcEnchantAdd#add_attr.gerExtraDemageRate - TarEnchantAdd#add_attr.gerDecDemageRate) / 10000
        				+ (SrcAttackAdd + TarDamageAdd2-TarDamageDec2) / 10000)))} 
    end.

damage_stastic(Pos,Damage) ->
	case ?get_ger(Pos) of
		#ger{gerID=GerID} ->
			NewDamage = abs(Damage) + get_damage_cc(GerID),
			set_damage_cc(GerID,NewDamage);
		_ ->
			ok
	end.

get_damage_cc(GerID) ->
	case get(damage_cc) of
		L when is_list(L) ->
			case lists:keyfind(GerID, 1, L) of
				false ->
					put(damage_cc, [{GerID,0}|L]),
					0;
				{_,Damage} ->
					Damage
			end;
		_ ->
			put(damage_cc, [{GerID,0}]),
			0
	end.

set_damage_cc(GerID,NewDamage) ->
	L = get(damage_cc),
	put(damage_cc,lists:keyreplace(GerID, 1, L, {GerID,NewDamage})).

add_legend_buff_for_all_ger()->
	FighterInfoList = ?get_alives(),
	[add_legend_buff_for_single_ger(Pos)||#fighter_info{pos=Pos}<-FighterInfoList].

add_legend_buff_for_single_ger(Pos)->
	Ger=#ger{gerID=GerID} = ?get_ger(Pos),
	case ?get_ger_legend_add(GerID) of
		?undefined->
			ignore;
		Add->
			Ger = ?get_ger(Pos),
			NewGer = ger_attr:add_legend_rank_add(Ger,Add),
			?set_ger(NewGer)
	end.
