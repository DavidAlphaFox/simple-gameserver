%% 战斗中经常用到的精灵数据, 位置、技能、绝技附加效果
-record(fighter_info, {pos,skill,speed=0}).

%% 精灵的动态属性,当前的属性值, buffer列表
-record(dynamic_buffer, {attr=#add_attr{}, buffers=[]}).

%% 不变参数
-record(arg_env, {roleID=0, isChange=false, fighterList=[], from, ref, queue=[],attTr,dfdTr,dramaID=0}).

-define(ger_morph_status_normal, 0).
-define(ger_morph_status_sleep, 1).
-define(ger_morph_status_sing, 2).
-define(ger_morph_status_dance, 3).

%% 绝技附加的效果(运算时使用)
-record(unique_effect,
            {
                damage_plus={0,[],[]},            %%伤害加成
                damage_sub={0,[]},             %%伤害减免
                atk_dmg_sub={0,[],[]},            %%攻击造成的伤害减少
                atk_dmg_plus={0,[],[],[]},           %%攻击造成的伤害增加
                critic_plus=0,            %%绝技暴击率
                hit_plus=0,               %%命中加成
                invinciblep=false,        %%是否是无敌的
				inv_way={[],[]},				  %% 无敌触发方式
                cure_sub=0,               %%治疗降低
                dot=[],                   %%DOT伤害[(施加者,伤害,持续时间)]
                damage_link1={0,[]},            %%伤害链接分摊的百分比,施法者列表
				damage_link2={0,[]},				
                damage_link3={0,[]},
                sleep=0,                  %%对沉睡的目标造成N倍伤害(持续性的,入场加上后不会消失的)                   
                weak=[],                  %%对血量少于X%的造成N被伤害(持续性的,入场加上后不会消失)
                weakest=0                 %%增加的虚弱攻击的个数
				,target_add=[]			  %% 普攻和必杀目标+N
				,hit_critic={false,[]}		  %% 必定命中且暴击
				,damage_add={0,[]}			  %% 被敌方伤害提高万份比,施法者列表
				,attack_add=0			  %% 对敌方攻击提高万份比
				,damage_dec=0			  %% 被地方攻击降低万份比
				,cure_add=0				  %% 治疗增加
				,crit_hit_add=0		      %% 暴击伤害增加
                ,confuse_inc=0            %% 增加的迷惑个数
                ,in_rage=false            %% 是否处于暴怒状态
				,damage_base_blood=0	  %% 造成基于血量N%的伤害
                ,morph_status=0           %% 状态
                ,morph_has_status=[]      %% 精灵拥有的状态数(目前就歌唱和跳舞使用)
                ,morph_info=[]            %% 变身相关的数据,格式:{morph_status, 数据}
                ,morph_sleep_critic=0     %% 沉睡反击暴击
				,damage_double=0		  %% 伤害翻倍
				,dead_mark=0			  %% 死亡标记数量
				,holy_kill={0,0}		  	  %% 必杀秒杀
				,damage_maxHp=0			  %% 附带生命上限N%的伤害
				,ningdong={0,0}			  %% 凝东信息  {凝冻次数, buff值}
				,is_cp=false			  %% 是否发生了复制, 一般精灵是false,可复制而未完成复制为0,复制完成后为复制的gerTypeID
				,con_attack=false		  %% 是否多段攻击
				,no_injury=0			  %% 免伤
				,hurt_cure=[]			  %% 治疗
				,skill_ra_up=[]				  %% 技能增幅
				,parry	= false 				%% 格挡
                ,spRa = 100                     %%  回能百分比
                ,maxCount = 4                   %%  最大连击数量 
                ,superCrit = {0,1}                  %% 超级暴击 ,{概率,伤害倍数}
                ,pm_dmg_dec = 0                     %% 奥义护盾减伤
            }).

%% 绝技附加效果记录
-record(effect_record,
            {
                effectId=0                 %%效果ID
                ,duration=0                %%有效回合
				,trSkill=0                 %% [{skillID,BufValue,SpecialID}]              
                ,subtype=undefined         %% 子类型
        }).


%% 附加技能效果的buffer
-record(unique_buffer, 
            {
                use=#unique_effect{},   %%所有effect的效果和
                effects=[]              %%存放effectId
            }
       ).

%% 追加攻击的record
-record(append_attack, 
            {
                type = undefined,       
                %% normal               普通攻击
                %% hit_critic           必定命中和暴击的普通攻击
                %% unique               绝技攻击
                %% normal_dot           普通攻击+dot伤害
                %% speed                急速
                %% unique_critic        必杀暴击
                target = [],            %% 作用对象
                args = []               %% 参数
            }
        ).

%% 绝技需要特殊处理的地方
-record(uh,
            {
                c1  = [],               %% 第一个是attack后处理,第二个是攻击完成后处理
                c2  = []
            }
        ).

%% 攻击返回的数据
-record(attack_info,                    %% 战斗相关数据
            {
                action  = [],
                uaction = [],           %% 绝技action
                dmgPlus = [],
                miss    = [],           %% 这个用miss_unmiss_acc更准确
                absorb  = 0,
                dmgBack = 0,
                unhandleds = #uh{},   
                append = [],            %%这次攻击产生的追加攻击的信息,三头龙允许一次触发两个追击,所以这儿要改为列表了
                use_append = #append_attack{}, %% 进行追加攻击时使用的信息
                trigger = [],           %% 已经触发了的绝技附加技能
                type    = [],           %% 攻击类型
                count   = 0,            %% 攻击次数记录
                damage  = 0,            %% 此次攻击造成的伤害
                direct  = 0,            %% 直接伤害,无视技能等其他属性应该打出的伤害
				holy_kill_list = [],			%% 秒杀列表
                atk_skill   = undefined,    %% 攻击用到的技能
                nstrike_back=[],        %% 普通反击数据
                ustrike_back=[],        %% 绝技反击
                mstrike_back=[],        %% 沉睡变身
                multirecord=[],          %%用来记录trigger里面不记录,但后面要用到的技能
                afteraction=[],          %% 记录对每个精灵产生的动作，在所有的动作的放完之后，释放的动作比如爆炸效果，该效果由于需要记录应该造成的伤害，所以在对精灵造成伤害后就产生并且记录
				tarGerType=0,			%% 被攻击目标的type
				no_injury_list=[],		%% 免伤列表
				hurt_cure_list=[],		%% 治疗列表
				confuse_list=[]			%% 记录本回合被判定confuse的人
                ,maxCount = 5
            }
        ).

%% 必杀附加技能触发条件分类
-record(ut,
           {
                always=[],              %% 始终触发 
                hit = [],               %% 命中时触发
                miss = []               %% 未命中时触发
            }
        ).

%% 必杀附加技能的触发数据
-record(unique_trigger,
            {
               t1   = [],               %% 放必杀前触发的
               t2   = #ut{},            %% 放必杀后触发的
               t3   = [],               %% 追加的攻击释放前
               t4   = #ut{},            %% 追加的攻击释放后
               t5   = [],               %% 必杀暴击
               t6   = [],               %% 被击中后
               t7   = [],               %% 必杀造成伤害后
               t8   = [],               %% 训练师火系攻击占用了
               t9   = [],               %% 攻击动作结束后
			   t10  = [],				%% 进入无敌触发
			   t11  = [],				%% 解除无敌触发
			   t12  = [],			    %% 反击后触发
			   t13  = [],				%% 普通攻击后
			   t14  = [],				%% 释放normal_dot后
			   t15  = [],				%% 释放unique_critic后 
			   t16  = [],				%% unique 释放触发
			   t17  = [],				%% 追击释放触发
			   t18	= [],				%% unhandled 影响的技能
			   t19 	= []				%% 所有动作结束且没有其他append动作的非unique_critic时触发
				,t20=[]					%% 任意动作前
				,t21=[]					%% 任意攻击前
				,t22=[]					%% 发生伤害后
				,t23=[]					%% 发生伤害后
				,t24=[]					%% 必杀攻击后
                ,t25=[]                 %% 眩晕状态受到攻击后
            }
        ).

%% buff回合记录
-record(maimed,
{
    id = 0,
    type = 0,
    srcpos = 0,
    tarpos = 0,
    duration = 0,
    effectId = 0
}).

-record(trBuff, {type=0,buff=0,modulusBase=0,modulusCo=0,target=0,duration=0,special=0}).

-record(fi_param,{d_minHp=-1,cur_limit=0}).

%%%============================================================================
%%%宏
-define(BEFORE_ATTACK_HOOK,1).
-define(ATTACK_HOOK,2).
-define(AFTER_ATTACK_HOOK,3).
-define(REBORN_HOOK,4).
-define(KILLED_HOOK,5).
-define(DEAD_HOOK,6).
-define(ENTHER_HOOK,7).
-define(NEW_LOOP_HOOK,8).
-define(MAX_APPEND_ATTACK,5).
-define(UNIQURE_APPEND_LIMIT, 4).

-define(get_ger(Pos), erlang:get(Pos)).
-define(set_ger(Ger), erlang:put(?b(Ger,gerPos),Ger)).
-define(get_unique_buffer(Pos), erlang:get({unique_buffer,Pos})).
-define(set_unique_buffer(Pos,Buffer), erlang:put({unique_buffer,Pos},Buffer)).
-define(get_damage_link(Pos), erlang:get({damage_link,Pos})).
-define(set_damage_link(Pos,Link),erlang:put({damage_link,Pos},Link)).

-define(set_reborn(Pos), erlang:put({reborn,Pos},1)).
-define(is_reborn(Pos), (erlang:get({reborn,Pos}) =/= undefined)).
-define(get_god_skill(Pos), erlang:get({god,Pos})).

%% 残废相关
-define(get_ger_maimed(Pos), erlang:get({maimed,Pos})).
-define(set_ger_maimed(Pos,Info), erlang:put({maimed,Pos},Info)).
-define(add_ger_maimed(Pos, ID), ?set_ger_maimed(Pos, [ID|?get_ger_maimed(Pos)])).
-define(delete_ger_maimed(Pos, ID), ?set_ger_maimed(Pos, lists:delete(ID, ?get_ger_maimed(Pos)))).
-define(is_maimed(Pos), ?get_ger_maimed(Pos) =/= []).
-define(get_target_maimed(Pos),erlang:get({target_maimed,Pos})).
-define(set_target_maimed(Pos,Info),erlang:put({target_maimed,Pos},Info)).
-define(delete_target_maimed(Pos,ID), ?set_target_maimed(Pos, lists:delete(ID, ?get_target_maimed(Pos)))).
-define(get_maimed(ID), erlang:get({maimed_id, ID})).
-define(set_maimed(ID, Maimed), erlang:put({maimed_id, ID}, Maimed)).
-define(delete_maimed(ID), erlang:erase({maimed_id, ID})).
-define(get_maimed_id(), erlang:get(maimed_id)).
-define(set_maimed_id(ID), erlang:put(maimed_id, ID)).

%% 其他计数,但非残废相关的buff
-define(get_ger_other_maimed(Pos), erlang:get({other_maimed, Pos})).
-define(set_ger_other_maimed(Pos, Info), erlang:put({other_maimed, Pos}, Info)).
-define(delete_ger_other_maimed(Pos, ID), ?set_ger_other_maimed(Pos, lists:delete(ID, ?get_ger_other_maimed(Pos)))).
-define(add_ger_other_maimed(Pos, ID), ?set_ger_other_maimed(Pos, [ID|?get_ger_other_maimed(Pos)])).

-define(physic_defence,(?calc_defence((SrcAttr#gerAttr.gerPhyDefBite + DataSkill#data_skill.gerPhyDefBite - TarAttr#gerAttr.gerPhyDef)))).
-define(magic_defence,(?calc_defence((SrcAttr#gerAttr.gerMagDefBite + DataSkill#data_skill.gerMagDefBite - TarAttr#gerAttr.gerMagDef)))).
-define(calc_defence(V),(if V =< 0 -> 1+V*4/(1200-V*5);true -> (V*5+1200)/(V+1200) end)).

%% 绝技
-define(get_unique_hook(Pos), erlang:get({unique_hook, Pos})).
-define(set_unique_hook(Pos, Hook), erlang:put({unique_hook, Pos}, Hook)).

%% 生成必杀选择列表
-define(make_select_list(Pos,List), [Pos|lists:delete(Pos,List)]).

-define(ACTION_FROZEN_ID,                          40).            %%冰冻
-define(ACTION_FROZEN_ID_1,                          236).            %%冰冻
-define(ACTION_REEL_ID_1,                          232).            %%眩晕
-define(ACTION_PARALYSIS_ID,                       42).            %%麻痹
-define(ACTION_PARALYSIS_ID_1,						248).			%% 麻痹
-define(ACTION_EXPLOSED_ID,                        44).            %%爆炸
-define(ACTION_SPEED_ID,                           45).            %%急速
-define(ACTION_ALLURE_RMV_ID,                      138).           %%诱惑ID 
-define(ACTION_FREEZE_1,							221).			%% 冰封
-define(ACTION_FREEZE_2,							223).			%% 强化冰封


-define(get_alives(), erlang:get(alives)).
-define(set_alives(Alive), erlang:put(alives, Alive)).
-define(delete_alive(Pos), ?set_alives(lists:keydelete(Pos, #fighter_info.pos, ?get_alives()))).
-define(get_queue(), erlang:get(queue)).
-define(set_queue(Queue), erlang:put(queue, Queue)).
-define(set_loop_queue(Queue), erlang:put(loop_queue, Queue)).
-define(get_loop_queue(), erlang:get(loop_queue)).

%%皮肤相关
-define(set_attack_skin(SkinInfo),(erlang:put(attack_skin,SkinInfo))).
-define(get_attack_skin(),(erlang:get(attack_skin))).
-define(set_defend_skin(SkinInfo),(erlang:put(defend_skin,SkinInfo))).
-define(get_defend_skin(),(erlang:get(defend_skin))).

-define(set_skin_info(Sign,SkinInfo),(erlang:put({skin_info,Sign},SkinInfo))).
-define(get_skin_info(Sign),(erlang:get({skin_info,Sign}))).
-define(set_skin_buff(Sign,SkinBuff),(erlang:put({skinBuff, Sign},SkinBuff))).
-define(get_skin_buff(Sign),(erlang:get({skinBuff,Sign}))).

-define(set_attack_skinbuff(SkinBuff),(erlang:put(attack_skinbuff,SkinBuff))).
-define(get_attack_skinbuff(),(erlang:get(attack_skinbuff))).
-define(set_defend_skinbuff(SkinBuff),(erlang:put(defend_skinbuff,SkinBuff))).
-define(get_defend_skinbuff(),(erlang:get(defend_skinbuff))).
-define(set_pos_skinbuff(Pos,SkinBuff),(erlang:put({skin_buff,Pos},SkinBuff))).
-define(get_pos_skinbuff(Pos),(erlang:get({skin_buff,Pos}))).

%%传奇装备buff
-define(set_ger_legend_add(GerID,Add),(erlang:put({legend_add,GerID},Add))).
-define(get_ger_legend_add(GerID),(erlang:get({legend_add,GerID}))).

-define(d_minHp,d_minHp).
-define(cur_limit, cur_limit).
%%v3.4.0增加准备列表用于计算传奇装备锻造加成
-record(double_team,{skin_info=[],talent=[],trainer=#trSpecial{},fighters=[],lieu=#add_attr{},itemList=[]}).

-define(clear_morph_info(Key, Info), proplists:delete(Key, Info)).
-define(get_morph_info(Key, Info), proplists:get_value(Key, Info)).
