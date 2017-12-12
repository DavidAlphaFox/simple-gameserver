%% key-value结构
-record(kv, {
			 key :: term()				% 键
			 ,value :: term()			% 值
			}).

%%神器系统信息
-record(holyGrail,{holyGrailLevel=1,isFinishSacrifice=false,diamondInfo=[]}).

%% 主公属性结构
-record(role,{
			  %% 基础属性
			  roleID :: ?int32
			  ,accid :: ?int64
			  ,roleName="" :: ?string
			  ,isMale=true	:: boolean()
			  ,description="" :: ?string
			  ,familyID=0 :: ?int32
              ,lastJoinFamily=0 :: ?int32
			  %% 成长属性		
			  ,level=1 :: ?int16
			  ,exp=0 :: ?int64
			  %% 货币管理
			  ,coin=0 :: ?int32
			  ,reputation=0 :: ?int32
			  ,gold=0:: ?int32
			  ,goldBonus=0 :: ?int32
			  ,goldUsed=0 :: ?int32
              ,unioncoin=0 :: ?int32  % 公会货币
              ,profoundCrystal=0 :: ?int32 %奥义结晶
			  %% 充值vip
			  ,vipLevel=0 :: ?int8
			  ,goldTotalPaid=0 :: ?int32
			  ,title=0 :: ?int8				% 官爵
			  ,fightPower=0 :: ?int64		% 总战斗力
			  ,lastLogoutTime=0				% 上次下线时间 
			  ,head=0 :: ?int32					%选取头像   0:默认头像
              ,payExtReward=0  :: ?int32       % 充值额外奖励
			  ,extRdActTime=0	::?int32		%充值额外奖励的活动开始时间
			  ,location="" ::?string
              ,isFailed=false :: boolean()
			  ,deviceID="" :: ?string
              ,srcType=0 :: ?int16
			  ,lastLoginTime=0 ::?int16 %%上次上线时间
              ,tasklevel = 0 ::?int16 %%筛选任务用的等级，每天0点更新一次
              ,teamId = -1 ::?int16 %%玩家组队id，未组队时为-1
              ,plane_level = 1 ::?int16 %%卡洛斯飞机的等级,初始就是1级的飞机
              ,honor=0 :: ?int32 %荣誉
              ,carloswintime=0 :: ?int32 %卡洛斯胜利次数
              ,carlosequaltime=0 :: ?int32 %卡洛斯平局次数
              ,carloslosetime=0 :: ?int32 %卡洛斯失败次数
              ,carlosseasonid=0 :: ?int32 %卡洛斯数据所属赛季,由Year+SeasonID组成
              ,carloslastwintime=0 :: ?int32
              ,carloslastequaltime=0 :: ?int32
              ,carloslastlosetime=0 :: ?int32
              ,carloslastseasonid = 0 :: ?int32
              ,pvppoint=0 :: ?int32 
              ,home_resource=0 :: ?int32
			  ,firstPayStatus=0 :: ?int8
			  ,ticket=0 :: ?int32
			  ,laputastone=0 :: ?int32
              ,transmigration=0 :: ?int32
              ,sGoldTotalPaid=0 :: ?int32 %% super vip gold paid count
              ,svipLevel=0 :: ?int8
			 }).

%% 玩家公共属性
-record(rolePublic, {
					 roleID
					 ,isMale
					 ,level
					 ,title
					 ,roleName
					 ,fightPower
					 ,lastLogoutTime
					 ,goldTotalPaid
  					 ,head
  					,location
					,viplevel
					,familyID
  					,plane_level
  					,trSpecial
                    ,transmigration
                    ,svipLevel
                    ,xbattleChapterID = 0
					}).

%% 背包武将结构
-record(gerSimple, {
					gerID	:: ?int32							%   武将唯一ID
					,gerTypeID :: ?int16                        % 模版ID
					,gerQuality:: ?int16                   	    % 品质
					,gerLevel :: ?int8                          % 等级
					,gerExp :: ?int64                           % 经验
					,gerPos=0::?int8							% 在队伍中的站位
					,gerAwakeInfo=[]                            % 精灵觉醒信息
					,gerCrystalInfo=-1                          % 精灵晶体信息
                    ,gerBody = 0 :: ?int16                      % 精灵变身状态
                    ,gerHolyGrailInfo=#holyGrail{}              % 精灵神器系统信息
				   }).

%% 武将基础属性：实例化属性，根据这些属性，可以完整算出武将的其他属性
-record(gerBase, {
				   gerTypeID :: ?int16                         % 模版ID
				  ,gerQuality:: ?int16                   	   % 品质
				  ,gerLevel :: ?int8                           % 等级
				  ,gerPos :: ?int8                             % 在队伍中的站位
				  ,gerExp :: ?int64                            % 经验
				  ,gerAwakeInfo=[]                             % 精灵觉醒信息
				  ,gerCrystalInfo=-1
                  ,gerBody = 0 :: ?int16                       % 精灵变身状态
                  ,gerHolyGrailInfo=#holyGrail{}               % 精灵神器系统信息
				 }).

%% 武将战斗属性，重算属性时经常修改的
-record(gerAttr, {                             
				   gerProMean = 0 :: ?int16                    % 精灵的奥义护盾
				  ,gerProMeanAddtion = 0 :: ?int16             % 奥义值增加百分比（用万分比表示）
				  ,gerAttckToProMean  = 0 :: ?int16            % 攻击转化为奥义的百分比（用万分比表示）
				  ,gerHpToProMean = 0 :: ?int16                % 生命转化为奥义的百分比（用万分比表示）
				  ,gerAttack =0:: ?int32                         % 攻击力
				  ,gerHpMax =0:: ?int64                          % 血量上限
				  ,gerSpInit =0:: ?int16                         % 怒气初始值
				  ,gerSpMax =0:: ?int16                         % 怒气最大值
				  ,gerCritic =0:: ?int16                         % 暴击
				  ,gerCriticReduce =0:: ?int16                   % 暴击抵抗
				  ,gerDoom =0:: ?int16                           % 命中
				  ,gerMiss =0:: ?int16                           % 闪避
				  ,gerAbsorb =0:: ?int16                         % 吸血
				  ,gerDamageBack =0:: ?int16                     % 反弹
				  ,gerReel =0:: ?int16                           % 眩晕
				  ,gerReelReduce =0:: ?int16                     % 眩晕抵抗
				  ,gerPhyDefBite=0:: ?int16	         	   	   % 破甲
				  ,gerPhyDef=0:: ?int16                		   % 护甲
				  ,gerMagDefBite=0:: ?int16                 	   % 法穿 
				  ,gerMagDef=0:: ?int16                 		   % 法抗
				  ,gerFightPower=0:: ?int64					   % 战斗力
                  ,gerProHpMax=0:: ?int64                      % 奥义护盾最大值
				  ,gerSpeed=0::?int16							%% 速度
				 }).

%% 武将所有属性
-record(ger,{
			 gerID	:: ?int32								% 武将唯一ID
			 ,gerBase :: #gerBase{}							% 武将基础属性
			 ,gerAttr	:: #gerAttr{}						% 武将战斗属性
			 ,gerExtra =0:: any()						    % 存战斗时配置属性
			 ,gerHp :: ?int64                               % 当前血量
			 ,gerSp :: ?int32                               % 当前怒气
             ,gerProHp :: ?int64                            % 当前奥义护盾
			 ,gerFamilyTekAdd :: #add_attr{}                % 当前科技对武将加成
             ,gerMagicBookAdd :: #add_attr{}                % 魔典加成
             ,gerEnchantAdd :: #add_attr{}                  % 附魔加成
             ,gerAwakeAdd ::  #add_attr{}                   % 觉醒加成
             ,gerCrystalAdd :: #add_attr{}                  % 晶体加成
			 ,gerSpeed :: ?int16							% 速度
			}).

%% 武将技能
-record(gerSkill,{
				  normal :: [?int16]						% 普通技能
				 ,unique :: [?int16]						% 无双技能/绝世技能
				 ,wake	 :: [?int16]						% 觉醒技能
				 ,god	 :: [?int16]						% 神将技能
				 ,enter  :: [?int16]						% 登场技能
				 ,god2	 :: [?int16]						% 圣将技能
				 ,buff   :: [?int16]                        % 带来属性增强的buff技能
				 %,other	 :: [?int16]						% 其他技能
				}).

%% 体力相关
-record(roleTimes, {
					energy=0			    :: ?int16			% 体力	
					,challengeGodEnergy=0	:: ?int16			% 神将挑战次数
					,challengeGodBuyTimes=0	:: ?int16			% 神将购买次数
					,lastChallengeGodDate={2013,1,1} ::calendar:date() % 上次领取免费挑战神将录次数的时间
					,refreshLieuTimes=0::?int16					% 副将格子免费刷新次数
					,alreadyPayRefreshLieuTimes=0::?int32		% 使用元宝刷新副将的次数		
					,energyBuyTimes=0	:: ?int16			% 体力已购买次数
					,dscvBuyTimes=0	:: ?int16			% 探索已购买次数
					,pvpBuyTimes=0	:: ?int16			% 争霸已购买次数
					,coinBuyTimes=0::?int16					%银两购买次数
					,fireTimes=0::?int32					%放鞭炮次数
					,lastBuyTimesRefreshDate={2013,1,1} :: calendar:date() % 上次刷新各种体力购买次数的日期		
					,lastEnergyTime=0    :: ?int32			% 上次体力恢复时间 
					,discoveryTimes=0	:: ?int8			% 探索次数
					,lastDscvTime=0	    :: ?int32			% 上次探索次数恢复时间
					,dscvCount=0			:: ?int32		% 探索总计数
					,pvpTimes=0	:: ?int16					% pvp当前剩余次数
					,weiboCount=0 :: ?int8					% 微博分享次数
					,nextWeiboCountRefreshSec=0 :: ?int32 	% 下次刷新微博分享次数的时间
					,lastWeiXinShareSec=0 ::?int32			% 上次微信分享时间
					,lastPvpTime=0		::	?int32			% 上次pvp次数恢复时间
                    ,leftChgNameTimes=0 ::?int8                 % 已改名次数 
                    ,talentstudyBuyTimes=0   :: ?int16           % 天赋增加已购买次数
                    ,teamPkTimes=0           ::?int16       % 3v3剩余次数
                    ,teamPkBuyTimes=0        ::?int16       % 3v3已购买次数
                    ,lastTeamPkTime=0        ::?int32       % 上一次恢复3v3次数的时间
                    ,energyPac=0                ::?int16    % 体力存储
				   }).

%% 体力类道具使用信息
-record(item_use_info, {
                        itemTypeID=0              :: ?int16              %道具模版ID
                        ,useDate={0,0,0}          :: calendar:date()     %最近一次使用日期
                        ,useTimes=0               :: ?int8               %当日累积使用次数
                        }).

%% 遭遇战章节信息
-record(echapter, {
				  id :: ?int16									%% 以玩家ID加章节ID为key
				  ,endTimerRef=0 :: timerRef()|0				%% 章节关闭倒计时
				  ,dungeonList=[] :: [#p_edungeon{}]|?int32 	%% 遭遇战关卡结构
				  ,collectEndTime=-1 :: ?int32					%% 章节征收、探宝倒计时
				  ,isCollected=false	:: boolean() 			%% 是否 已经征收、或者夺宝过				  
				  }).


%% 每日奖励信息
-record(daily, {
				lastTitleRewardDate=0 :: calendar:date()|0		% 上次领取官爵奖励的时间，创角时为0
			   ,lastDrawTitle=0:: ?int8							% 上次领取官爵奖励时的官爵，创角时为0
			   ,lastLoggedLoginDate=0 :: calendar:date()|0		% 上次记录下来的登录日期，创角时为0
			   ,lastDrawLoginRewardDays=0 :: ?int32				%上次领取连续登录奖励的连续登录日数
			   ,loginDays=0 :: ?int32							%当前的连续登录日数
			   ,lastDrawLevelUpLevel=0	::?int16				%上次领取的升级礼包的等级
			   ,lastPayTime=0 ::?int32                          %上次充值的时间
				}).
  
%% 卡片信息
-record(card, {
			   groupID	% 卡牌所在的group
			   ,type    % 卡牌类型
			   ,value   % 卡牌数值
			  }).
%% 点将信息
-record(cardInfo, {
			   openedCardList=[] :: [#p_opened_card{}] % 已抽取的卡牌
			   ,cardList =[]	:: [#card{}]		   % 等待抽取的列表	
			   ,drawCount=1		:: ?int32			   % 当前的总点击次数
				,activityID=0		:: ?int16				% 当前的活动ID
			   }).

%% 武将站位信息
-record(posInfo,{
				 gerID
				 ,gerPos
				 ,gerTypeID
				}).

%% 华容道信息
-record(hronInfo, {
				   date :: calendar:date()					% 数据日期，活动每天开一次，每天的数据对应一个日期,验证使用
				   ,star :: 1..4							% 4个星级
				   ,curDungeonNum :: ?int16					% 当前第几关，0=当前未开始挑战
				   ,attackAdd::?int16						% 当前攻击增加百分比
				   ,hpAdd::?int16							% 当前血量增加百分比 
				   ,morale::?int16							% 士气
				   ,dungeonIDList::[?int16]					% 随机出来的关卡列表
				   ,bestScore								% 最高关数 
				   ,challengeTimes							% 已挑战次数
                   ,isHaveSuccReward=0 :: ?int16            % 是否有通关奖励,0为没有
                   ,lastFightResult=0 :: ?int8              % 上一次的战况，0为上次没有打，1为赢，2为输
				   ,isSelect=0 :: ?int8						% 是否选择了星级
				  }).

%% 限制开启信息
-record(limitInfo,{
				   encounterNum=0 :: ?int16					% 奇遇解锁数量
				   ,isBindWeibo=false :: boolean()			%是否绑定了微博	
				   ,inviteRoleID=0 :: ?int32				%邀请你的玩家ID
				   ,inviteRoleName="" :: ?string			%要请你的角色名称
				   ,lastShareLevel=0::?int16				%上次微博分享的主公等级
				   ,spiritGoldBoxCount = 0::?int32				%付费元宝固定次数单抽宝箱计数
				   ,spiritGoldBonusBoxCount=0::?int32				%赠送元宝固定次数单抽宝箱计数
				   ,spiritItemBoxCount = 0:: ?int32				%道具抽取次数
				   ,equipGoldBoxCount = 0::?int32				%付费元宝固定次数单抽宝箱计数
				   ,equipGoldBonusBoxCount=0::?int32				%赠送元宝固定次数单抽宝箱计数
				   ,equipItemBoxCount = 0:: ?int32				%道具抽取次数
                   ,trainerGoldBonusBoxCount=0:: ?int32
                   ,trainerGoldBoxCount=0:: ?int32
                   ,trainerItemBoxCount=0:: ?int32
				   ,spiritRefresh=0				%精灵抽卡刷新
				   ,equipRefresh=0				%道具抽卡刷新
				   ,trainerRefresh=0			%训练师抽卡刷新
				  ,combine2StarCount=0			%2星合成
				   ,ticketBoxCount=0 ::?int32	%点券抽卡计数
					,ticketBoxCount2=0 :: ?int32 % 十连抽点券技术
 				  }).

%% 限制开启信息
-record(gerMirrorInfo,{
                   gerMirrorState={#gerSimple{},0} 
                   ,gerMirrorTimeList=[] 
                  }).

%% 玩家的武将相关信息
-record(d_ger, {
				roleID		:: ?int32
				,posList :: [#ger{}]			% 站位列表
				,gerList :: [#gerSimple{}]		% 武将列表
			   }).

%% 玩家额外的无交互属性
-record(roleExtra, {
					roleID :: ?int32						% 玩家ID
					,battleProgress	:: ?int32				% 战役进度
					,battleProgressHard ::?int32			% 炼狱战役进度
					,battleProgressFastHard ::?int32	%最难困难副本
                    ,battleProgressTransmigration ::?int32
					,roleTimes	:: #roleTimes{}		% 体力系统相关
					,encounterList :: [#echapter{}]		% 遭遇战列表
					,shopNumList :: [#p_shop_num{}] % 商品已购买次数
					,dailyInfo :: #daily{} % 每日奖励信息
					,cardInfo :: #cardInfo{} % 点将信息
					,hronInfo :: #hronInfo{} % 华容道信息		
					,limitInfo ::#limitInfo{} % 限制开启信息
					,gatherList::[tuple()] % [武将图片sets，道具图片sets]
					,randomShopList::[#p_shop_random{}] % 奇遇商店列表
                    ,itemUseList::[]    %道具使用信息列表
                    ,signSec :: ?int32 %% 注册时间
				   }).
					
%% 道具结构
-record(item, {
			   itemUID	:: ?int64		
			   ,itemTypeID	:: ?int16						% 道具模版ID
			   %,itemName 	:: ?string						% 道具名称
			   ,itemType	:: 								% 道具类型
				   		weapon									% 武器
					   |armor									% 盔甲
					   |horse									% 坐骑
					   |treasure_physical_damage_addtion		% 各种宝物
					   |treasure_physical_damage_reduction
					   |treasure_magic_damage_addtion
					   |treasure_magic_damage_reduction
					   |treasure_critic
					   |treasure_critic_reduce
					   |treasure_doom
					   |treasure_miss
					   |treasure_sp_init
					   |treasure_sp_left
					   |treasure_absorb
					   |treasure_damage_back
					   |treasure_reel
					   |treasure_reel_reduce
					   |treasure_tianqian
					   |treasure_dikun
					   |treasure_leizhen
					   |treasure_fengxun
					   |treasure_shuikan
					   |treasure_huoli
					   |treasure_shangeng
					   |treasure_zedui
					   |material
					   |patch_treasure
					   |other
					   |soul_general
					   |debris_weapon
					   |debris_armor
					   |debris_horse
					   |box
			   %,itemStar	:: ?int8						% 道具初始星级
			   ,itemLevel	:: ?int8						% 道具强化等级
			   ,itemRank	:: ?int8						% 道具初始品阶
			   ,itemNum	:: ?int16						% 道具数量
			   ,itemPos :: ?int8						% 装备在身上=1-11.不在身上=0
			   ,itemDecay:: timerRef()|0				% 下次品阶衰减时间
			   ,addAttr	:: #add_attr{}|0					% 加成的属性,只有在身上的装备，此字段才有值，背包中和其他敌方的道具的此字段均为0
			   ,itemExp :: ?int16						% 宝物类型道具的经验
			   ,itemenchantType =0 :: ?int8                 %装备附魔类型
			   ,itemenchantLevel=0 :: ?int8                %装备附魔等级
			   ,itemLegendRank=0 ::?int16               % 传奇锻造等级
			  }).					

%% 宝物碎片结构
-record(patch, {
				 typeID	:: ?int16					% 道具模版ID
				,num	:: ?int16					% 道具数量
				}).


%% 邮件
-record(mail, {
			   mailUID	::?int64%邮件唯一ID
			   ,mailType::?int8		%邮件类型
			   % 1=> 系统消息                               
			   % 2=> 系统奖励                               
			   % 3=> 加好友申请                              
			   % 4=> 加联盟申请                              
			   % 5=> 私人邮件                               
			   % 6=> 联盟邮件                               
			   ,senderID=0::?int32		%邮件发送者的UID，0=系统发送的
			   ,senderName=""::?string		%发送者名字,如果本邮件是系统邮件，则此字段为空
			   ,content=""::?string			%内容
			   ,time=0::?int32			%发送时间(Unix时间戳)
			   ,mailTemplateID=0::?int16	%邮件模版ID
			   ,paramList=[]::[any()]		%动态类型参数列表
			   ,mailReward=[]	%可领取奖励
			   ,isRead=false :: boolean() % 是否被阅读过
			   ,head=0
			   ,isMale=false
			  })                                                                                                                                                 .

%% 邮件数据库结构
-record(d_mail, {
				 roleID :: ?int32
				 ,mail=[[],[],[]] :: [[#mail{}]]	%% 三种类型的邮件列表
				 ,unreadNum=[0,0,0]	::[?int32]%% 三种类型的邮件的未读数量
				}).

%% 好友数据库结构
-record(d_friend, {
				   roleID :: ?int32
				   ,pal=[] ::[?int32]	% 朋友 	
				   ,foe=[] ::[?int32]	% 仇人
				  }).

%% 邀请码相关的玩家数据
-record(d_invite, {
				   roleID :: ?int32					% 角色ID
				  ,inviteRoleIDList :: [?int32]		%邀请者列表
				  ,rewardNum						%领到的奖励数量
				  }).

%% 推送相关的玩家数据
-record(d_push, {
				 roleID
				 ,token			:: binary()				%推送用的Device Token 参数
				 ,isPVPPushOpen	:: boolean()			%争霸推送是否开启
				 ,isPushNightMute :: boolean()			%晚上是否免打扰
				}).

-record(draw,{
			  drawID
			  ,alreadyDrawTimes
			  ,ableDrawTimes
			 }).

%% 活动结构
-record(act,{
			 actID
			 ,value
			 ,list
			}).
%% 玩家活动相关数据
-record(dc, {
			 roleID
			 ,actList
			}).
%% 宝物碎片结构
-record(t_patch, {roleID	::?int32
				,p_21201	::?int16
  				,p_21202	::?int16
  				,p_21203	::?int16
  				,p_21204	::?int16
  				,p_21205	::?int16
  				,p_21211	::?int16
  				,p_21212	::?int16
  				,p_21213	::?int16
  				,p_21214	::?int16
  				,p_21215	::?int16
  				,p_21216	::?int16
  				,p_21217	::?int16
  				,p_21218	::?int16
  				,p_21219	::?int16
  				,p_21220	::?int16
  				,p_21231	::?int16
  				,p_21232	::?int16
  				,p_21233	::?int16
  				,p_21234	::?int16
  				,p_21235	::?int16
  				,p_21241	::?int16
  				,p_21242	::?int16
  				,p_21243	::?int16
  				,p_21244	::?int16
  				,p_21245	::?int16
  				,p_21251	::?int16
  				,p_21252	::?int16
  				,p_21253	::?int16
  				,p_21254	::?int16
  				,p_21255	::?int16
  				,p_21261	::?int16
  				,p_21262	::?int16
  				,p_21263	::?int16
  				,p_21264	::?int16
  				,p_21265	::?int16
  				,p_21266	::?int16
  				,p_21267	::?int16
  				,p_21268	::?int16
  				,p_21269	::?int16
  				,p_21270	::?int16
				}).

%% encounter Chapter List 结构
-record(t_encounter,{attackTimes
  					,chapterID
  					,dungeonID
  					,fightInfo
  				    ,monTotalHp
  					}).

%% 参军的格子配置信息
-record(t_lieu,{pos
				,infoID1
  				,isLock1
  				,infoID2
  				,isLock2
  				,infoID3
  				,isLock3}).

%% 联盟成员信息
-record(family_member_info,{
    role_id=[] :: ?int32
    ,role_name=[] :: ?string
    ,family_id=[] :: ?int32
    ,family_contribution=[] :: ?int32
    ,left_family_contribution=[] :: ?int32
    ,use_gold_time=[] :: ?int32
    ,title=[] :: ?int8
    ,is_male=[] :: boolean()
    ,online=[] :: boolean()
    ,role_level=[] :: ?int16
    ,fight_power=[] :: ?int64
    ,family_title=0 :: ?int8
	,is_join_war=0 :: ?int8 %% 0 => 未参战 1 => 已参战
	,attack_times=0 :: ?int16
	,defend_times=0 :: ?int16
	,win_star=0 :: ?int16
	,reward_level=0 ::?int8 %% 膜拜次数,利用原来公会每日的字段,减少修改
    ,join_time=0 :: ?int32
	,weeklyContributes=0 :: ?int32
	,lastContributeDate=0 :: calendar:date()
	,recvEnergyList=[]
	,storageReqData={[],{{1970,1,1},0}}
    ,head=0 :: ?int32   %% 头像
	,offline_time=0 :: ?int32   %% 最后下线时间
    ,limit_shop=[] ::[]
    ,anubisinfo=[]
    ,vip=0
    }).
%% 联盟战对手信息
-record(family_fight_other_info,{war_period=0 :: ?int32
								 ,is_sign=0 :: ?int8
								 ,attack_times=0 :: ?int32
								 ,defend_times=0 :: ?int32
								 ,win_star=0 :: ?int32
								 ,last_world_rank=0::?int32
								 ,fight_result=0::?int8 %% 1 => win 3 => eq 2 => lose 0 => no
								 ,matcher_win_star=0::?int16
								 ,matcher_server_id=0::?int16
								 ,matcher_family_id=0 ::?int32
								 ,matcher_family_name=[]
								 ,matcher_family_rank=0::?int32
                         }).
%% 工会跨服战信息
-record(familycross_fight, {period=0,isSign=0,cars=[],disTime=0,score=0,enermy=[]}).

%% 联盟详细信息
-record(family_info,{
    family_id=[] :: ?int32
    ,family_name=[] :: ?string
    ,level=[] :: ?int16
    ,create_role_id=[] :: ?int32
    ,create_role_name=[] :: ?string
    ,owner_role_id=[] :: ?int32
    ,owner_role_name=[] :: ?string
    ,cur_members=[] :: ?int16
    ,active_points=[] :: ?int32         %% 联盟建设值
	,family_score=[] :: ?int32			%% 联盟战总分
    ,notice=[] :: ?string				%% 公告
    ,slogan=[] :: ?string				%% 口号
    ,members=[] :: [#family_member_info{}]
    ,rank=[] :: ?int32
	,world_rank=[] :: ?int32
	,is_zero_refreshed=[] ::calendar:date()
	,family_fight_other_info=[] :: #family_fight_other_info{}
    ,create_time=[] :: ?int32
    ,talk_data=[]
	,log_data=[]
	,contribute_log=[]
    ,leftChgNameTimes=0 ::?int8 %% 已改名次数
    ,family_task=[]
    ,cross_rank=0                       %% 全服排名
	,talkRoomID="0"						%% 语音房间ID
    ,family_instance_state=[]
    ,family_fighter_group=[]
	,familycross_fight_info=#familycross_fight{} :: #familycross_fight{}
    ,family_anubisinfo=[]
}).

%% 联盟限制商店
%%-record(family_limit_shop,{
%%	shopID=0 :: ?int8
%%	,levelLimit=0 :: ?int8
%%	,buyTimesLimit = 0 :: ?int8
%%	,usedTimes=0 :: ?int8
%%	,cost = 0 :: ?int8
%%	,reward=[] :: ?int8
%%}).



%% 公会副本信息
-record(family_instance_state,{
    is_win = false
   ,next_instance =0
   ,inst_list=[] 
   ,cur_inst_boss = []  
   ,fight_member_times=[]  
   ,reward_get_status=[] 
   ,extra_reward_is_get = 0
   ,damage_list=[]
}). 

%% 公会科技等级信息表
-record(inst_boss_info,{
    index = 0
   ,max_hp = 0
   ,cur_hp = 0
   ,boss_list = []
   ,killer_role_id = 0
    }).

%% 联盟申请加入信息
-record(family_request,{
    role_id=[] :: ?int32
    ,role_name=[] :: ?string
    ,level=[] :: ?int16
    ,fight_power=[] :: ?int64
    ,timestamp=[] :: ?int32
    ,family_id=[] :: ?int32
    ,head=[] :: ?int32
    ,title=0 :: ?int8             % 官爵
    ,is_male=[] :: boolean()}).

%% 公会科技等级信息表
-record(family_technology,{
	familyID=0 ::?int32,
	familyTekList=[] ::[?int32]
	}).
%% 联盟战对手信息
-record(family_match, {server_id=0 :: ?int32
					  ,family_id=0 :: ?int32
					  ,time_stamp=0 :: ?int32
					  ,family_name=[]::?string
					  ,fighter_info_list=[#p_familyfighter_member_info{}]
					  ,locked_list=[]	  
					  }).
%% 联盟战报名信息
-record(pf_fighter,{familyID=0, serverID=0, score=0,pareID=0,result=0,star=0,matchFamilyID=0, matchServerID=0,matchStar=0,ownerName=""
				   ,level=0,rank=0,lastRank=0,familyName="",matchFamilyName="",matcherRank=0,matcherTotalFightPower=0,totalFightPower=0}).

%% 汉帝宝库数据配置
-record(treaHouseInfo, {
						activityID,
						value_info,
						card_list,        %% 随机生成的随机奖励
						free_count,       %% 累积的免费探索次数
						buy_count,        %% 累积的付费探险次数
						free_times,       %% 剩余的免费次数
						mark,             %% 探险值
						baseBoxGetList,
						isGetRankReward
						}).

%% 返利信息结构
-record(rebate_info,{id				%% 返利ID
					 ,amount		%% 消费数量[银两，元宝，声望]
					 ,get			%% 是否已经领取奖励
					}).

-record(friend_enargy,{
		roleID,
		toFriendList,				%%赠送好友体力列表
		toMeList,						%%好友赠送给自己体力列表	
		addFriendList,				%%加好友请求列表记录		
		giveTimes,					%%今日领取次数
		date								%%日期
}).

-record(alien_info,{times=0,
                    lastRecoverTime=0,
                    resetTime=0}).

%% family teck
-record(familyTek, { familyID=0							:: ?int32
					,tekList=[]							:: [#p_familyTekDtl{}]
					}).
%% family wallet
-record(family_wallet,{rice=0							:: ?int32}).
%% family storage
-record(p_storageInfo, {id=0,typeID=0,num=0,reqList=[]}).

%%　免费抽卡相关
%%-record(box_open_info,{lastGerTime=0,lastItemTime=0,lastTrainerTime=0}).
-record(box_open_info,{gerTime1=0,gerTime2=0,itemTime1=0,itemTime2=0,trainerTime1=0,trainerTime2=0}).

%%玩家公会捐献信息
-record(family_contribution,{role_id=0,diamondNum=0,coinNum=0,reputationNum=0,gerdonateinfo=[],itemdonateinfo=[],donateContribution=0}).

%%精灵或者道具捐献
-record(donateunit,{star=0,donateNum=0}).

%%保存玩家分解所得物品，用于再次分解
-record(decompose_reward,{recordID = 0,addcoin=0,addgold=0,addgerexp=0,addroleexp=0,addreputation=0,addprofoundCrystal=0,gerlist=[],itemlist=[],decomposegerlist=[],decomposegerequiplist=[],decomposestonelist=[],decomposetrainerlist=[]}).

%%限时打折活动信息
-record(d_discount_info,{
	activitylist=[]
	}).

%%具体活动配置信息
-record(d_discount_activity_unit,{
	activityID=0,activity_condition=[],real_activityID=0,begintime=0,endtime=0
	}).

%%用于排序捐献查询的临时结构
-record(temp_contribution_unit,{memberinfo=0,contributioninfo=0}).

%%保存玩家满足限时打折充值活动的形象
-record(satisfied_discount_pay_info,{activityID=0,ranklist=[]}).

%%保存玩家能够领取的活动
-record(pay_activity_can_get_unit,{roleID=0,activityList = []}).

%%保存某一个活动能够领奖的玩家列表
-record(pay_activity_can_get_list,{activityID=0,rolelist=[]}).

%%保存全区抢购活动的配置信息
-record(panic_buy_activity_info,{activityID=0,beginTime=0,endTime=0,personBuyTime=0,totalBuyTime=0,nextdecreasetime=0,decreasedtime=0}).

%%全区抢购从服务器信息
-record(slave_node_info,{serverID=0,node=0}).

%%全区抢购活动玩家记录
-record(panic_buy_activity_record,{id=0,begintime=0,endtime=0,buytime=0,personbuytime=0,totalbuytime=0}).

%%月卡
-record(monthVIP_info, {buyBigSec=0,buyLittleSec=0,lastGetBigSec=0,lastGetLittleSec=0,restBigDays=0,restLittleDays=0,todayPayBig=0,todayPayLittle=0}).

%%组队信息
-record(team_info,{teamid=0,teamleader_roleid=0,vice_teamleader_rolelist=[],team_member=[],teamtype=1,teamstatus=0}).

%% 幸运转盘
-record(lucky_roll,{activityID=0,free_count=0,buy_count=0,free_times=0
			   ,mark=0,isGetRankReward=0,baseBoxInfo=[],outer=[],inner=[],pos=#p_lucky_role_card_p{pos1=0,pos2=0}}).

% vip shop timestamp 最后更新时间
-record(vip_shop,{activityID=0, items=[], timestamp=0}).

% 魔典状态
-record(magicBook_state,
        {
            summary = "",
            picture_state = [],
            percent = []
        }).

%% 神兵天降
-record(tvcard,{
		activityID=0,
		openID=1,
		cards=[]		
}).

% 训练师专精
-record(trSpecial,{
				   trID = 0
				  ,specialID = 0
				  ,roleLevel = 0
				  ,sp = 0
				  ,state = 0
				   }).

%% 精灵觉醒结构
-record(awake,{step=0,skillID=0,recast_time=0,skill_quality=0,new_skilllist=[]}).

%% 精灵觉醒阶段配置
-record(step,{stepid=0,skillIDList=[]}).

%% 玩家签到信息
-record(sign_info,{sign_time_count=0,is_get_sign_reward=0,last_sign_time=0,is_get_acc_sign_reward=0}).

%% 成长计划记录
-record(growth_fund_record,{is_buy=0,reward_get_list=[]}).

%% vip设置结构
-record(vip_info,{roleid=0,vip=0}).

%%训练师皮肤
-record(skin_buff,{
					ger_attack_plus=0,                      %%羁绊精灵伤害提高
					ger_demage_sub =0,                      %%羁绊精灵伤害降低
					trainer_mark_plus=0,                    %%训练师标记增加
					even_trainer_demage_plus=0              %%训练师偶数回合伤害增加
	}).

%%单个皮肤的结构
-record(skin,{id=0,arank=0,activateIDList=[]}).

%%玩家皮肤数据
-record(skin_info,{has=[],equip=0}).

%%皮肤增加的buff
-record(skin_fetter_buff,{buff=#skin_buff{},fetterGerList=[]}).

-record(bounty_info, {time_limit=0
                     ,last_login={0,0,0}
                     ,bountyData=[]}).
                     
%%砸金蛋数据
-record(role_egg_info,{roleID=0,score=0,validtime=0,times=0,item_use_list=[]}).

%%双排队伍信息,team_memberlist保存doublematch_teammemberinfo列表
-record(doublematch_teaminfo,{teamid=0,team_memberlist=[]}).

-record(doublematch_lineup_gerinfo,{flage=0,ger=#ger{}}).

%%双排队员信息,需要注意的地方是由于布阵的过程中，需要精灵有队长和非队长的区别，所以fighters列表中为#doublematch_lineup_gerinfo{}方式保存
%%v3.4.0加入装备列表,用于计算传奇锻造加成
-record(doublematch_teammemberinfo,{roleID=0,skin_info=[],talent=[],trainer=#trSpecial{},fighters=[],lieu=#add_attr{},authority_rank=0,fight_turns=0,itemList=[]}).

%% 卡洛斯飞机选择
-record(plane_info,{type=0,state=0,valid=0}).
-record(plane_use_info, {planes=[],use=0}).
%-record(plane_use_info, {planeType=0, planeType2=0,validTime=0,validTime2=0,use=0,planeType3=0,validTime3=0,planeType4=0,validTime4=0}).

%%单个晶体信息
-record(crystal,{crystaltype=0,crystalquality=0,crystallevel=0,crystalexp=0,crystalrankexp=0}).

% 家园boss攻打次数
-record(homeBoss_times,{total=0,goldTimes=0}).

% 抽卡信息
-record(shop_box_card,{cdInfo=0,openedCardInfo=[]}).% {type,id,rewardList}

%战役BOSS宝箱领取信息(增加这一步，用于扩展其他难度的战役能开同样的宝箱)
-record(battle_boss_reward_info,{battletype=0,chapterlist=[]}).

%每个章节的boss宝箱领取信息
-record(chapter_boss_reward_info,{chapterID=0,dungeonlist=[]}).

%关卡的boss宝箱领取信息
-record(dungeon_boss_reward_info,{dungeonID=0,rewardstatus=0}).

%%无尽深渊历史信息
-record(hron_history_info,{difficultytype=0,bestscore=0}).

%%阿努比斯赛季信息
-record(anubis_season_info,{seasonid=0,seasonrank=[],rankfamilynum=0,send_reward_flage=0,delete_flage=0}).

%%赛季排行分段
-record(anubis_rank_seg,{segid=0,segmin=0,segmax=1,segnum=0,seglist=[]}).

%%排行榜上榜数据
-record(family_player,{familyid=0,familyname=0,familyserver=0,familyleader=0,familyrank=0,familyscore=0,familykillnum=0,familyfightpower=0,ischange=true,updatetime=util:now(),familylevel=0}).

%%战斗结果，其中memberresult代表公会成员战斗结果列表,resourcepoint表示占领的点，resource表示产生的资源
-record(anubis_fight_result,{fightresult=0,killnum=0,resource=0,memberresult=[]}).

%%公会成员战斗结果
-record(anubis_family_member_result,{roleid=0,killnum=0,resourcepoint=0}).


%%赛季表现， 用于记录公会以及公会成员在对应赛季的表现
% -record(anubis_representation,{seasonid=0,killnum=0,resource=0}).

%%赛季表现， 用于记录公会对应赛季表现
-record(anubis_family_representation,{seasonid=0,killnum=0,rank=0,score=0,updatetime=0}).

%%公会成员表现
-record(anubis_member_representation,{seasonid=0,killnum=0,resourcepoint=0}).

%%公会的阿努比斯赛季信息
-record(anubis_family_info,{current_season_id=0,representationlist=[]}).

%%玩家成长计划信息
-record(payGuideInfo,{currenttaskid=0,unaccepttasklist=[],acceptedtaskIDList=[],showtaskID=0}).

%% 节日签到
-record(activity_festival, {id=0,sign=[],box=[]}).

%% 技能宝石单元信息
-record(diamond_unit,{pos=0,diamondID=0}).

%% 技能宝石单元信息
-record(role_tasklink,{last_date=0,free_time=0,buy_left_time=0,pay_count=0}).
-record(tasklink_info,{role_id=0,leader_id=0,diffcult=0,start_timestamp=0,points=[],reward_log=[],next_timestamp=0,point_need_time=0,member=[],log_args=[]}).

%% explore boss
-record(role_exBoss,{bossID=0,curHp=0,maxHp=0,haveTimes=0,buyTimes=0,lastRefreshSec=0,isGetReward=0,bossLevel=0,oneHit = 0,hitList=[],nowHit=0,maxHit=0,freeRefresh=0,reward=#sell_reward{},cdduration=1}).

%% xbattle
-record(role_xbattle, {chapterID=0,rewardChapterID=0 ,buyQuickCount=0,buyQuickToday=0,quickFightSec=0,raidTimes=0,challengeCount=0,lastTriggerSec=0,offlineInfo={0,0,#sell_reward{}},passData=[]}).
-record(xbattle_chapter, {chapterID=0
                          ,passDungeons=[] %% {dungeonID,passTimes}
                          ,isGetReward=0
                          ,challengeCount=0
                         ,passCount=0}).

%%玩家头七活动信息
-record(head_seven,{begintime=0,period=0,doingtask=[],finishtask=[],finishrewardtask=[],nextperiod_ref=undefined,nextperiodtime=0,isfirst=0}).

%%头七活动任务
-record(head_seven_task_unit,{taskID=0,finishprogress=0,type=0,state=0,period=0}).

%%新图鉴单元
-record(manual_unit,{gerTypeID=0,collect=0,tag=0}).

%%收集任务单元
-record(manual_collect_task,{taskID=0,tag=0,finish=0,state=0,type=0}).

%%新图鉴信息
-record(manual_info,{manual_unit_list=[],manual_collect_task_list=[]}).

%%主线任务状态
-record(main_task_info,{maintaskID=0,state=0,finish=0}).

%%道馆状态
-record(dojang_info,{pass_id_list=[]
                    ,date={0,0,0}
                    ,harvest_free_time=0
                    ,harvest_pay_time=0
                    ,buy_time=0}).

-record(treasurebowl_activity,{activityID=0,drawlist=[],activitystate=0}).
-record(treasurebowl_drawunit,{drawID=0,period=0,state=0}).

%%红包数据
-record(golder_info,{golder_roleid = 0
                    ,redpacket_num = 0          %% 本次活动获得的红包
                    ,redpacket_day = {0,{0,0,0}}          %% 本次活动获得的红包
                    ,redbox_list = []
                    ,last_pay_time = 0}).   %% [#redbox_info{}]

%%训练师职业
-record(trainer_prof,{type=0,level=0}).

%%培育室信息
-record(trainer_rear_machine,{machineID=0,type=0,objectList=[],matureTime=0,state=0,matureTimeRef=?undefined,matureCostTime=0}).

%% dojangrank
-record(role_dojangrank,{day={0,0,0}
                        ,free_time=0
                        ,paid_time=0
                        ,buy_time=0
                        ,local_fight_rec_list = []
                        ,world_fight_rec_list = [[],[],[],[],[],[],[],[]]
                        ,selected_ger_type_list = []
                        ,world_free_time = [0,0,0,0,0,0,0,0]
                        ,world_paid_time = [0,0,0,0,0,0,0,0]
                        ,world_buy_time = [0,0,0,0,0,0,0,0]
                        ,world_enemy_list = [[],[],[],[],[],[],[],[]]
                        ,world_refresh_time = [0,0,0,0,0,0,0,0]}).

-record(dojangrank,{rank=0
                   ,role_id=0
                   ,selected_ger_type = 0
                   ,fighter_power=0
                   ,fighter_array_cfg=[]}).  %% fighter_array_id可能是数字，也可能是list(robot的情况)

-record(camp_unit,{camp_type=0,gernum=0,normaladd={0,0},specialadd=#add_attr{},germaxnum=0}).

-record(camp_add,{totalnormaladd={0,0},totalspecialadd=#add_attr{}}).

%% 签到  
%% signMonth
-record(sm,{mon=0,data=0,tto=0,rwd=0}).
-record(daySign,{mData=[],bsDay=0,s7c=0,tto=0,lmSec=0,lmReward=[]}).

%% ets表中的数据单元结构
-record(drw_rank_data, {rank = 0
                       ,role_id = 0
                       ,server_id = 0
                       ,dr_info = #p_dr_world_rank{}
                       ,fighter_data = {}}).    %%{FighterListA,RoleLieuAddA,TalentA,TrSpecialA,SkinInfoA,LegendAddListA}

%%训练室
-record(playerTrainingRoom,{
	chapterID=0,%%对应章节ID
	leftHP=0%%BOSS剩余血量
}).

%%consumerank_serversend_reward处使用了这个结构体来ets:select 没有直接使用record ，如果更改这个地方，必须修改那个地方
%%新服玩家充值消费记录
-record(consume_pay_unit,{roleID=0,isMale=false,level=0,title=0,roleName=0,fightPower=0,head=0,vip=0,consume=0,pay=0,rank=0}).