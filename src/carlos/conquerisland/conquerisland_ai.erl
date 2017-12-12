-module(conquerisland_ai).
-include("common.hrl").
-include("def_carlos.hrl").
-export([]).

-define(condition_any, 0).      %% 其他并列条件不符合，剩余默认处理的情况
-define(condition_alive, 1).    %% 是否还活着
-define(condition_mine_outside, 2). %% 是否在岛屿外
-define(condition_mine_self, 3).    %% 所在岛屿是否是己方占领的
-define(condition_mine_getting, 4). %% 岛屿是否在占领中
-define(condition_mine_have_enemy, 5).  %% 岛屿中是否存在其他敌人
-define(condition_mine_me_alone, 6).
-define(condition_mine_no_move, 7).
-define(condition_target_self, 8).
-define(condition_target_getting, 9).
-define(condition_not_start, 21).
-define(condition_mine_have_buffes, 11). %% 拥有的buff数量达到一定程度
-define(condition_is_move_boss, 12).     %% 是不是向boss移动

-define(act_wait, 1001).
-define(act_ownMine, 1002).
-define(act_move_type1, 1011).
-define(act_move_type2, 1012).
-define(act_attack_type1, 1021).

-define(pd_robot_list, pd_robot_list).
-define(pd_player_list, pd_player_list).        %% 用于寻找挂机者
-define(pd_start_timestamp, pd_start_timestamp).

-define(ATTACKER_BOSS_TYPE,1).
-define(DEFENDER_BOSS_TYPE,2).

init(PlayerListAll)->
    {PlayerList,RobotList} = 
        lists:foldl(fun(P,{AccPlayerList,AccRobotList})->
                if
                    P#conquerisland_player.roleID > ((P#conquerisland_player.serverID+1)*1000000) andalso 
                    P#conquerisland_player.roleID < (((P#conquerisland_player.serverID+1)*1000000)+10000) ->
                        put({pd_operate_ts,P#conquerisland_player.roleID},util:now()+3600),       %% 避免机器人显示挂机
                        {AccPlayerList,[{P#conquerisland_player.roleID,P#conquerisland_player.serverID}|AccRobotList]};
                    true->
                        put({pd_operate_ts,P#conquerisland_player.roleID},util:now()),       %% 避免机器人显示挂机
                        {[{P#conquerisland_player.roleID,P#conquerisland_player.serverID}|AccPlayerList],AccRobotList}
                end
            end, {[],[]}, PlayerListAll),
    ?INFO("PlayerList:~w  ++  RobotList:~w",[PlayerList,RobotList]),
    put(?pd_start_timestamp,util:now()),
    put(?pd_player_list,PlayerList),
    put(?pd_robot_list,RobotList).

get_arrived_mine({_RID,_SID},_Type,[])->
    ?undefined;
get_arrived_mine({RID,SID},Type,[MineID|OtherMineIdList])->
    MineData = conquerisland_war_server:get_centre(MineID),
    case conquerisland_war_server:is_in_centre(SID,RID,MineData) of
        {true,_Type} ->
            MineData;
        false ->
            get_arrived_mine({RID,SID},Type,OtherMineIdList)
    end.

get_can_attack_mine(_Type,[],AccList)->
    AccList;
get_can_attack_mine(Type,[MineID|OtherMineIdList],AccList)->
    MineData = conquerisland_war_server:get_centre(MineID),
    if
        MineData#centre.ownerType /= Type ->
            get_can_attack_mine(Type,OtherMineIdList,[MineID|AccList]);
        true ->
            get_can_attack_mine(Type,OtherMineIdList,AccList)
    end.

check_robot_act()->
    AfkLimit = util:now() - data_conquerisland:get(afk_check_time),
    PlayerList = get(?pd_player_list),
    AfkPlayerList = 
        lists:filter(fun({RID,_SID})->
                        AfkTime = get({pd_operate_ts,RID}),
                        AfkTime =:= ?undefined orelse AfkTime < AfkLimit
                end, PlayerList),
    RobotList = get(?pd_robot_list),
    AiList = AfkPlayerList ++ RobotList,
    AiConfig = [{?condition_not_start,?act_wait}
               ,{?condition_alive,?act_wait}
               ,{?condition_any,[{?condition_mine_outside,[{?condition_mine_have_buffes,[{?condition_is_move_boss,?act_wait}
                                                                                        ,{?condition_any,?act_move_type2}]}
                                                          ,{?condition_any,[{?condition_mine_no_move,?act_move_type1}
                                                                           ,{?condition_is_move_boss,?act_move_type1}
                                                                           ,{?condition_target_self,?act_move_type1}
                                                                           ,{?condition_target_getting,?act_move_type1}
                                                                           ,{?condition_any,?act_wait}]}]}
                                ,{?condition_any,[{?condition_mine_have_enemy,?act_attack_type1}
                                                 ,{?condition_any,[{?condition_mine_self,?act_move_type1}
                                                                  ,{?condition_mine_getting,?act_move_type1}
                                                                  ,{?condition_any,?act_ownMine}]}]}]}],
    lists:foreach(fun({RID,SID})-> 
                          PlayerData = conquerisland_war_server:get_player(SID,RID),
                          MineData = get_arrived_mine({RID,SID},PlayerData#conquerisland_player.type,data_conquerisland:get(centre_list)),
                          check_robot_act2({RID,SID},PlayerData,MineData,AiConfig) 
                  end, AiList).

check_robot_act2(RSID,PlayerData,MineData,[{ConditionType,NextLogic}|OtherAiNode])->
    ?INFO("check_robot_act2 -~w->>>>~w   ,,,   ~w",[RSID,{ConditionType,NextLogic},MineData]),
    case check_condition(ConditionType,RSID,PlayerData,MineData) of
        true->
            %% NextLogic 可能是某个动作，或者是分支条件 
            check_robot_act2(RSID,PlayerData,MineData,NextLogic);
        false ->
            check_robot_act2(RSID,PlayerData,MineData,OtherAiNode)
    end;
check_robot_act2(RSID,PlayerData,MineData,ActType) when erlang:is_integer(ActType)->
    do_act(ActType,RSID,PlayerData,MineData).    

check_condition(?condition_not_start,_RSID,_PlayerData,_MineData)->
    Now = util:now(),
    StartTs = get(?pd_start_timestamp) + data_plane_ai:get(conquerisland_wait_time),
    Now =< StartTs;
check_condition(?condition_any,_RSID,_PlayerData,_MineData)->
    true;
%% 1)是不是死了
check_condition(?condition_alive,_RSID,PlayerData,MineData)->
    Now = util:now(),
    false =:= conquerisland_war_server:is_player_alive(PlayerData);
%% 2)判断是不是在岛屿外
check_condition(?condition_mine_outside,_RSID,PlayerData,MineData)->
    MineData =:= ?undefined orelse PlayerData#player.startPos /= PlayerData#player.endPos;
%% 3)所在岛屿是否是已经被己方占领的
check_condition(?condition_mine_self,_RSID,PlayerData,MineData)->
    Now = util:now(),
    MineData /= ?undefined andalso MineData#centre.ownerType =:= PlayerData#conquerisland_player.type andalso MineData#centre.state =:= 2;
%% 4)所在岛屿是否是正在被己方占领中
check_condition(?condition_mine_getting,_RSID,PlayerData,MineData)->
    Now = util:now(),
    MineData /= ?undefined andalso MineData#centre.snatcherType =:= PlayerData#conquerisland_player.type andalso MineData#centre.state =:= 1;
%% 5)所在岛屿中是否存在其他敌人
check_condition(?condition_mine_have_enemy,_RSID,PlayerData,MineData)->
    MineData /= ?undefined andalso 
        ((PlayerData#conquerisland_player.type =:= ?ATTACKER_BOSS_TYPE andalso MineData#centre.defenderList /= []) orelse
         (PlayerData#conquerisland_player.type =:= ?DEFENDER_BOSS_TYPE andalso MineData#centre.attackerList /= []));
%% 6)所在岛屿中是否存在其他敌人
check_condition(?condition_mine_me_alone,_RSID,PlayerData,MineData)->
    case PlayerData#conquerisland_player.type of
        ?ATTACKER_BOSS_TYPE when MineData /= ?undefined ->
            Num = erlang:length(MineData#centre.attackerList),
            Num =< 1;
        ?DEFENDER_BOSS_TYPE when MineData /= ?undefined ->
            Num = erlang:length(MineData#centre.defenderList),
            Num =< 1;
        true ->
            false
    end;
%% 7)是不是没移动
check_condition(?condition_mine_no_move,_RSID,PlayerData,MineData)->
    PlayerData#conquerisland_player.startPos =:= PlayerData#conquerisland_player.endPos;

%% 8)
check_condition(?condition_target_self,_RSID,PlayerData,_MineData)->
    Now = util:now(),
    TarMineData = conquerisland_war_server:get_centre(PlayerData#conquerisland_player.tarCentre),
    TarMineData /= ?undefined andalso TarMineData#centre.ownerType =:= PlayerData#conquerisland_player.type andalso TarMineData#centre.state =:= 2;
%% 9)
check_condition(?condition_target_getting,_RSID,PlayerData,_MineData)->
    Now = util:now(),
    TarMineData = conquerisland_war_server:get_centre(PlayerData#conquerisland_player.tarCentre),
    TarMineData /= ?undefined andalso TarMineData#centre.snatcherType =:= PlayerData#conquerisland_player.type andalso TarMineData#centre.state =:= 1;

%%
check_condition(?condition_mine_have_buffes,_RSID,PlayerData,MineData)->
    Now = util:now(),
    CentreList = 
        lists:foldl(fun(CentreID,AccList)->
                        Centre = conquerisland_war_server:get_centre(CentreID),
                        if
                            Centre#centre.ownerType =:= PlayerData#conquerisland_player.type andalso Centre#centre.state =:= 2 ->
                                [CentreID|AccList];
                            true ->
                                AccList
                        end
                    end, [], data_conquerisland:get(centre_list)),
    Num = erlang:length(CentreList),
    Num >= data_plane_ai:get(conquerisland_buff_num);
%%
check_condition(?condition_is_move_boss,_RSID,PlayerData,MineData)->
    ?undefined /= conquerisland_war_server:get_boss(PlayerData#conquerisland_player.tarCentre).

do_act(?act_wait,RSID,PlayerData,MineData)->
    ignore;
do_act(?act_ownMine,RSID,PlayerData,MineData)->
    {RoleID,ServerID} = RSID,
    ?INFO("--ai--(~w)占领~w",[RSID,MineData#centre.id]),
    conquerisland_war_server:do_centre_occupy(ServerID,RoleID,MineData#centre.id);
do_act(?act_move_type1,RSID,PlayerData,MineData)->
    case get_can_attack_mine(PlayerData#conquerisland_player.type,data_conquerisland:get(centre_list),[]) of
        [] ->
            ignore;
        List ->
            AttackMineId = util:random_one_from_list(List),
            {RoleID,ServerID} = RSID,
            ?INFO("--ai--(~w)向~w岛移动",[RSID,AttackMineId]),
            conquerisland_war_server:do_player_mov(PlayerData,AttackMineId)
    end;
do_act(?act_move_type2,RSID,PlayerData,_MineData)->
    [{AttackerBossID,_AttackerBossPos},{DefenderBossID,_DefenderBossPos}|_T] = data_conquerisland:get(bosslist),
    AttackMineId = case PlayerData#conquerisland_player.type of
                       ?ATTACKER_BOSS_TYPE -> DefenderBossID;
                       ?DEFENDER_BOSS_TYPE -> AttackerBossID
                   end,
    {RoleID,ServerID} = RSID,
    ?INFO("--ai--(~w)向~w岛移动",[RSID,AttackMineId]),
    conquerisland_war_server:do_player_mov(PlayerData,AttackMineId);
do_act(?act_attack_type1,RSID,PlayerData,MineData)->
    case MineData of
        ?undefined ->
            ignore;
        _ ->
            EnemyList = 
                case PlayerData#conquerisland_player.type of
                    ?ATTACKER_BOSS_TYPE ->
                        MineData#centre.defenderList;
                    ?DEFENDER_BOSS_TYPE ->
                        MineData#centre.attackerList
                end,
            case EnemyList of
                [] ->
                    ignore;
                _ ->
                    {TarServerID,TarRoleID} = util:random_one_from_list(EnemyList),
                    {RoleID,ServerID} = RSID,
                    ?INFO("--ai--(~w)向~w发起攻击",[RSID,{TarRoleID,TarServerID}]),
                    conquerisland_war_server:do_attack(ServerID,RoleID,TarServerID,TarRoleID,MineData#centre.id)
            end
    end.
    