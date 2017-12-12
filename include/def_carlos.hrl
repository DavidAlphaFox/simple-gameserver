-record(ets_carlos_info, {warID=0,pid=0,endTime=0,createTime=0}).
-record(player, {serverID=0,roleID=0,roleName=[],blood=0,fighters=[],baseFighters=[],addBuff=[],talent=[],itemList=[]   %9
				,startPos={0,0},endPos={0,0},tarMineID=0,startTime=0,fly=0,speed=1,replayList=[],type=0,rebornSec=0     %9
				,killNum=0,level=0,gas=0,score=0,head=0,title=0,isMale=0,movDis=0,trSpecial=0			                %8
                ,lastcarloswintime=0,lastcarlosequaltime=0,lastcarloslosetime=0,lastseasonid=0,carloswintime=0
                ,carlosequaltime=0,carloslosetime=0,seasonid=0 ,fight_power=0,skin_info=0,grade=0,vip=1}).

-record(getGas,{startGetSec=0,gas=0,getspeed=1}).%% 开始时间,采集量
-record(mine, {id=0,pos={0,0},owner=0,attackerList=[],defenderList=[],beOwnSec=0,lastOwner=0
              ,gas=0,attackerGas=#getGas{},defenderGas=#getGas{},totalGas=0,ownRole=0}).%% owner用0和1表示占领者是attacker还是defender

-record(ga_mine, {id=0,pos={0,0},attackerList=[],defenderList=[],startSec=0,gas=0,movTarget=0
                  ,home=0,maxNum=0,speed=1,defenderListW=[],attackerListW=[],score=0}).
-record(ga_home, {id=0, pos={0,0},type=0,mineIDs=[]}).

-record(ets_galactica_info,{warID=0,pid=0,endTime=0,createTime=0}).
-record(ga_player, {serverID=0,roleID=0,roleName=[],blood=0,fighters=[],baseFighters=[],addBuff=[],talent=[],itemList=[]   %9
					 ,startPos={0,0},tarMineID=0,startTime=0,fly=0,speed=1,replayList=[],type=0,rebornSec=0     %9
					 ,killNum=0,level=0,score=0,head=0,title=0,isMale=0,movDis=0,trSpecial=0,fight_power=0,skin_info=0,grade=0}).

-record(bossinfo,{bossId=0,bossHp=0,bossMaxHp=0,bossPos={0,0},bossLevel=100,type=0,demagePlus=0,hpPlus=0,critPlus=0,demageDec=0}).
-record(centre,{id=0,pos={0,0},ownerType=0,attackerList=[],defenderList=[],beOwnSec=0,snatcherType=0,ownerRole=0,snatcherRoleID=0,state=0,attackerNum=0,defenderNum=0}).

-record(ets_twins_info,{warID=0,pid=0,endTime=0,createTime=0}).
-record(tw_player, {serverID=0,roleID=0,roleName=[],blood=0,fighters=[],baseFighters=[],addBuff=[],talent=[],itemList=[]   %9
					 ,startPos={0,0},tarMineID=0,startTime=0,fly=0,speed=1   %9
					 ,level=0,score=0,head=0,title=0,isMale=0,movDis=0,trSpecial=0,fight_power=0,skin_info=0,grade=0}).

-record(ets_family_cross_info, {warID=0,pid=0, endTime=0,createTime=0}).
%% 联盟跨服战信息
-record(fc_fighter,{familyID=0,type=0, serverID=0,pareID=0,result=0,score=0,ownerName="",rank=0
				   ,lastRank=0,familyName="",totalFightPower=0,winScore=0,enermyInfo=[]}).

-record(familycross_player, {roleID=0,serverID=0,fighters=[],baseFighters=[],addBuff=[],talent=[],itemList=[],killNum=0
							 ,startPos={0,0,0},tarSite=0,city=0,startTime=0,replayList=[],rebornSec=0,roleName=[]
							 ,blood=0,flyType=0,level=0,isMale=0,movDis=0,trSpecial=0,skin_info=0,car=0,speed=0,type=0
							,head=0,title=0,fightPower=0,ownSiteTimes=0,familyID=0, score=0,grade=0}).
-record(city,{id=0,pos={0,0},cars=[],flys=[],sites=[],typeNum={0,0,0}}). 
-record(teamGas,{startGetSec=0,gas=0,getspeed=1}).%% 开始时间,采集量
-record(site,{id=0,cityID=0,pos={0,0,0},display=0,flys={[],[],[]},
			  owner=0,lastOwner=0,ownSec=0,team1Gas=#teamGas{},team2Gas=#teamGas{},team3Gas=#teamGas{}}).
-record(car,{id=0,type=0,carType=0,speed=1,players=[],driver=0,startPos={0,0},tarCity=0,startTime=0,driver2=0}).

%%使用base_player和extra数据来获取战斗需要的数据，从而避免多余数据的获取
-record(base_player,{serverID=0,roleID=0,roleName=[],roleLevel=0,head=0,title=0,isMale=0,fighters=[],addBuff=[],talent=[],itemList=[]   %9
                ,fly=0,trSpecial=0,grade=0
                ,fight_power=0,skin_info=0,type=0,speed=1}).

-record(conquerisland_player,{serverID=0,roleID=0,roleName=[],roleLevel=0,head=0,title=0,isMale=0,fighters=[],addBuff=[],talent=[],itemList=[]   %9
                ,fly=0,trSpecial=0,fight_power=0,skin_info=0,type=0,baseFighters=[],blood=0,startPos={p_pos,0,0},endPos={p_pos,0,0},tarCentre=0,startTime=0,baseSpeed=0,speedPlus=0,movDis=0
                ,occupyCentreTimes=0,killNum=0,bossDemage=0,score=0,rebornSec=0,replayList=[],fightPowerForBoss=0,bossfighttimes=0,grade=0}).
%% 报名请求
-record(request, {serverID=0, id=0, members=[], level=0}).

-record(tw_request,{serverID=0,id=0,members=[],level=0,type=0}).

%% 每一个角色的数据
-record(member_base_info,
                {
                    serverID = 0,                   
                    roleID = 0, 
                    head=0,
                    isMale=0,
                    title=0,
                    level=0,
                    fight_power=0   
                    %% levle 和 title会在room缺人申请时用到,但现在实现需求改了,实际用不到了
                    %% 但先保留着
                }
        ).

%% 每一个角色的数据
-record(f_member_base_info,
                {
                    serverID = 0,                   
                    roleID = 0, 
                    head=0,
                    isMale=0,
                    title=0,
                    level=0,
                    fight_power=0   
                    %% levle 和 title会在room缺人申请时用到,但现在实现需求改了,实际用不到了
                    %% 但先保留着
                }
        ).

%% 匹配的数据
-record(match, {team1=[], team2=[]}).   %%每一个都是carlos_role_info列表

%% 卡洛斯次数数据
-record(times, {
                    has_buy     = 0,    %% 购买增加的次数
                    buy_left    = 0,    %% 今日剩余购买次数
                    free_left   = 0,     %% 今日剩余免费次数
                    valid_timestamp = 0  %% 此记录的有效时间
                }).

%kaladijia
-record(ga_times, {
                    has_buy     = 0,    %% 购买增加的次数
                    buy_left    = 0,    %% 今日剩余购买次数
                    free_left   = 0,     %% 今日剩余免费次数
                    valid_timestamp = 0  %% 此记录的有效时间
                }).

-define(team_len(ID), ID band 2#111).
-define(make_id(TID,L), TID bsl 3 bor L).
-define(team_id(ID), ID bsr 3).
-define(generate_season_id(Year,SeasonID),(Year*100+SeasonID)).
-define(get_season_year(SeasonID),(SeasonID div 100)).
-define(get_season_seasonid(SeasonID),(SeasonID rem 100)).
-define(get_level(Lvl), (case Lvl > 999 of true -> 2; _ -> 1 end)).     %% v3.1.0 采用动态的等级匹配范围，只是用一个匹配循环所以这里写999
-define(get_lvl_info(Eterm,Lvl), (erlang:element(Lvl,Eterm))).
-define(set_lvl_info(Eterm,Lvl,Info), (erlang:setelement(Lvl,Eterm,Info))).
-define(incf_counter(Counter,Len), (setelement(Len + 1, Counter, element(Len + 1, Counter) + 1))).
-define(decf_counter(Counter,Pos,Need), (setelement(Pos, Counter, element(Pos, Counter) - Need))).
-define(processSeqNum, 2).
-define(max_wait_sec, 30).

-define(ai_flag_carlos, 1).
-define(ai_flag_galactica, 2).
-define(ai_flag_conquerisland, 3).

%% 传递到rank的玩家信息
-record(p_player_info,{serverID=0,roleID=0,roleName=[],fly=0,level=0,score=0,head=0,title=0,isMale=0,type=0,wintime=0,losetime=0,
                       equaltime=0,rankscore=0,rank=0,seasonid=0,lastwintime=0,lastequaltime=0,lastlosetime=0,lastseasonid=0
                        ,ischange=0,fight_power=0,vip=1}).

%% 
-record(rank_seg,{id=0,length=0,min=0,max=0,playerlist=[]}).

%记录一个赛季的排名信息,type表示是本赛季还是上赛季
-record(season_info,{seasonid=0,year=0,type=0,rankinfolist=[]}).

-define(ETS_RELIC_ROLE_INFO, ets_relic_role_info).%巨龙遗迹的玩家个人数据的数据集。ps. ets操作比list查找效率高
-define(ETS_RELIC_SIGN, ets_relic_sign).%缓冲数据无需保存，保存正在匹配的队伍ID与RoleID的对应关系。 启动时，根据ets_relic_role_info，生成缓存
-define(ETS_RELIC_PROCESS_INFO, ets_relic_process_info). %%  卡洛斯进程信息 战场id和pid的对应关系

-define(RELIC_SIGN_STATE_FIGHT, 1).
-define(RELIC_SIGN_STATE_NO_SIGN, 2).
-define(RELIC_SIGN_STATE_MATCH, 3).

-define(RELIC_ISLAND_STATE_BOSS, 1).
-define(RELIC_ISLAND_STATE_UNACTIVE, 2).
-define(RELIC_ISLAND_STATE_ACTIVED, 3).

-define(RELIC_FINAL_ISLAND_STATE_REBORN, 4).        %大boss是复活点
-define(RELIC_FINAL_ISLAND_STATE_BOSS_UNACTIVE, 5). %大boss出现未激活
-define(RELIC_FINAL_ISLAND_STATE_BOSS_ACTIVED, 6).  %大boss出现可攻击

-define(RELIC_REWARD_TYPE_FIGHT, 1).
-define(RELIC_REWARD_TYPE_KILL, 2).
-define(RELIC_REWARD_TYPE_WIN, 3).
-define(RELIC_REWARD_TYPE_WIN_OUTER, 4). %挂机胜利

-define(RELIC_WIN, 1).
-define(RELIC_LOSE, 2).

-define(RELIC_MATCH_SERVER_NAME(Num), list_to_atom(lists:concat(['relic_match_', Num]))).

-define(RELIC_LEVEL_NUM, 6).          % 难度数量,增加难度的话一定要改这里

-record(relic_role_data,{role_id=0
                        ,remain_fight_times=0
                        ,remain_buy_time=0
                        ,sign_state=0
                        ,war_id=-1
                        ,box_status=[0,0,0]
                        ,box_rank={0,0,0}
                        ,fight_end_ts=0
                        ,is_sleeper=false
                        ,other_list=[]
                        ,last_player=[]}).

-record(tw_times,{   has_buy     = 0,    %% 购买增加的次数
                    buy_left    = 0,    %% 今日剩余购买次数
                    free_left   = 0,     %% 今日剩余免费次数
                    valid_timestamp = 0,  %% 此记录的有效时间
					last_war_data = {0,0,0} ,%{难度,排名评价,类型}
					rank_data=[],
					twins_box_data_list = [] %% 奖励数据 p_twins_box_info
                }).


%% 匹配等待时的数据结构
%% status
%% 0 未准备
%% 1 已准备
-define(wait_status_cancel, 0).
-define(wait_status_ready, 1).
%% TODO 给客户端的数据应该用这个
-record(room_member,
                {
                    serverID=0,
                    roleID=0,
                    teamID=0,
                    head=0,
                    camp=0,     
                    tick=0,
                    isMale=0,
                    level=0,
                    fight_power=0,
                    title=0,
                    status=?wait_status_cancel
                }).

-record(room_bc_msg,
                {
                    roomID = 0,
                    roleIDs = [],
                    data = undefined
                }).

%% 匹配房间类型
-define(room_type_carlos, 0).
-define(room_type_relic, 1).
-define(room_type_galactica , 2).
-define(room_type_twins, 3).

-define(invalid_room_id, 0).
-define(get_room_type(RoomID), (RoomID bsr 28)).
-record(match_sign_info, 
                {
                    is_in_room = false,
                    data = 0
                }).

%% 日志相关 
%% 飞机类型
-define(carlos_type_carlos, 1).
-define(carlos_type_relic, 2).
-define(carlos_type_galactica, 3).
-define(carlos_type_twins, 4).
-define(carlos_type_conquerisland,5).

%% 操作类型
-define(carlos_op_sign, 1).
-define(carlos_op_unsign, 2).
-define(carlos_op_match_success, 3).
-define(carlos_op_enter_room, 4).
-define(carlos_op_exit_room, 5).
-define(carlos_op_room_success, 6).
-define(carlos_op_enter_war, 7).
-define(carlos_op_close_war, 8).
-define(carlos_op_time_match_success,9).
-define(carlos_op_time_buy,10).

