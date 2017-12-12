
% 无需保存至数据库，记录role进程的超时计时器
-define(sign_timeout_ref,sign_timeout_ref). 

-define(FRIEND_RANK,1).
-define(All_REGION_RANK,2).
-define(LOCAL_REGION_RANK,3).
-define(OLD_ALL_RANK,4).

-define(RANK_REC_LIST,1).
-define(ROLE_REC_LIST,2).

% dm_data是双排模块保存在内存中的数据格式，为了节约游戏服内存且保持数据最新，rolepublic的内容不重复使用
-record(dm_data,{role_id = 0        %% 
                ,server_id = 0      %% role所在服务器id
                ,index = 0          %% 玩家排名，没有排名的话，返回0
                ,remain_time = 0    %% 残余战斗次数
                ,already_buy = 0    %% 已经购买的次数
                ,rf_date = 0        %% 用于战斗次数的刷新
                ,rank = 0           %% 竞技场段位
                ,score = 0          %% 总积分
                ,session = 0        %% 记录玩家数据属于哪个赛季
                ,fight_rec = []
                ,remain_time_buy = 0    %% 记录购买的次数
                ,before_score = 0       
                }).

-record(dm_sign_info, {role_id = 0
                      ,server_id = 0
                      ,name = []
                      ,level = 0
                      ,isMale=true    :: boolean()    %% 用于显示头像的性别信息
                      ,title = 0          %% 用于显示头像
                      ,head = 0           %% 用于显示头像
                      ,double_team = #double_team{}
                      ,dm_data = #dm_data{}
                      ,vip=1
                      }).

%%v4.0版本将这个record定义从doublematch_server中移到此处
-record(dm_db_record, {role_id=0
                      ,dm_data=#dm_data{}
                      ,write_ts=0             %最后一次写数据是什么时间，0表示已经写入数据库了，已同步无需再写入
                      ,read_ts=0}).           %最后一次读数据是什么时间