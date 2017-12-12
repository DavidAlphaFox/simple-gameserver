
-define(STATUS_CLOSE,          0).  %% 关闭
-define(STATUS_SIGN,           1).  %% 报名
-define(STATUS_FIGHT,          2).  %% 比赛
-define(STATUS_FINAL,          3).  %% 决赛

-define(STATUS_FINALS_WAIT, 0).                 %% 等待期间
-define(STATUS_FINALS_PREOPEN, 1).              %% 准备开始
-define(STATUS_FINALS_GROUP_MATCH, 2).          %% 小组赛
-define(STATUS_FINALS_QUARTERFINAL, 3).         %% 四强赛
-define(STATUS_FINALS_SEMIFINALS, 4).           %% 半决赛
-define(STATUS_FINALS_FINAL, 5).                %% 总决赛

-record(role_alien, {roleID=0,is_sign=false, group_id=0, fighterList=[], itemList=[], fightPower=0, isMale=false, title=0, head=0,
                     level = 0, roleName = <<"">>, rank = 0, serverID = 0, hpPercent=100, killNum=0,
                     killContinuousNum=0, maxKillContinuousNum=0, isInContinuous=false,
                     guessCoin=0, guessType=false, timestamp=0,canBeAtkTime=0,vip=1,
                     %% 增加了天赋技能
                     atkAdd=0, hpAdd=0, talentList = [], lieuViewList=[],trSpecial=0,skin_info=0}).

-record(alien_fighter2,{
                        roleID=0
                        ,fightPower=0
                        ,isMale=false
                        ,title=0
                        ,head=0
                        ,level=0
                        ,roleName= <<"">>
                        ,rank=0
                        ,serverID=0
                        ,killNum=0
                        ,vip=1
                        ,timestamp=0}).

-record(alien_fighter3,{
                        roleID=0
                        ,fightPower=0
                        ,isMale=false
                        ,title=0
                        ,head=0
                        ,level=0
                        ,roleName= <<"">>
                        ,rank=0
                        ,serverID=0
                        ,killContinuousNum=0
                        ,isInContinuous=false,
                        vip=1
                        ,timestamp=0}).

-record(role_alien_final, {roleID=0
                            ,roleName= <<"">>
                            ,isMale=true
                            ,title=0
                            ,fightPower=0
                            ,head=0
                            ,level=0
                            ,serverID=0
                            ,groupID=0
                            ,fighterList=[]
                            ,lieuAdd=#add_attr{}
                            ,equiped_list=[]
                            %%1.4.0增加天赋技能
                            ,talentList=[]
						  ,trSpecial=0
                          ,skin_info=0
                          ,vip=1}).

