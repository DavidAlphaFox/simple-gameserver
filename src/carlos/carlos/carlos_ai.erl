-module(carlos_ai).
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
-define(condition_no_move, 7).
-define(condition_target_self, 8).
-define(condition_target_getting, 9).
-define(condition_not_start, 21).

-define(act_wait, 1001).
-define(act_ownMine, 1002).
-define(act_move_type1, 1011).
-define(act_attack_type1, 1021).

-define(pd_robot_list, pd_robot_list).
-define(pd_player_list, pd_player_list).        %% 用于寻找挂机者
-define(pd_start_timestamp, pd_start_timestamp).

init(PlayerListAll)->
    {PlayerList,RobotList} = 
        lists:foldl(fun(P,{AccPlayerList,AccRobotList})->
                if
                    P#player.roleID > ((P#player.serverID+1)*1000000) andalso 
                    P#player.roleID < (((P#player.serverID+1)*1000000)+10000) ->
                        put({pd_operate_ts,P#player.roleID},util:now()+3600),       %% 避免机器人显示挂机
                        {AccPlayerList,[{P#player.roleID,P#player.serverID}|AccRobotList]};
                    true->
                        put({pd_operate_ts,P#player.roleID},util:now()),       %% 避免机器人显示挂机
                        {[{P#player.roleID,P#player.serverID}|AccPlayerList],AccRobotList}
                end
            end, {[],[]}, PlayerListAll),
    ?INFO("PlayerList:~w  ++  RobotList:~w",[PlayerList,RobotList]),
    put(?pd_start_timestamp,util:now()),
    put(?pd_player_list,PlayerList),
    put(?pd_robot_list,RobotList).

get_arrived_mine({_RID,_SID},_Type,[])->
    ?undefined;
get_arrived_mine({RID,SID},Type,[Mine|OtherMineIdList])->
    {MineID,_,_} = Mine,
    MineData = war_server:get_mine(MineID),
    case war_server:check_mine_player(Type,{RID,SID},MineData) of
        true ->
            MineData;
        false ->
            get_arrived_mine({RID,SID},Type,OtherMineIdList)
    end.

get_can_attack_mine(_Type,[],AccList)->
    AccList;
get_can_attack_mine(Type,[{MineID,_,_}|OtherMineIdList],AccList)->
    MineData = war_server:get_mine(MineID),
    if
        MineData#mine.owner /= Type ->
            get_can_attack_mine(Type,OtherMineIdList,[MineID|AccList]);
        true ->
            get_can_attack_mine(Type,OtherMineIdList,AccList)
    end.

check_robot_act()->
    AfkLimit = util:now() - data_carlos:get(afk_check_time),
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
               ,{?condition_any,[{?condition_mine_outside,[{?condition_no_move,?act_move_type1}
                                                          ,{?condition_target_self,?act_move_type1}
                                                          ,{?condition_target_getting,?act_wait}
                                                          ,{?condition_any,?act_wait}]}
                                ,{?condition_any,[{?condition_mine_have_enemy,?act_attack_type1}
                                                 ,{?condition_any,[{?condition_mine_self,?act_move_type1}
                                                                  ,{?condition_mine_getting,[{?condition_mine_me_alone,?act_wait}
                                                                                            ,{?condition_any,?act_move_type1}]}
                                                                  ,{?condition_any,?act_ownMine}]}]}]}],
    lists:foreach(fun({RID,SID})-> 
                          PlayerData = war_server:get_player(RID,SID),
                          MineData = get_arrived_mine({RID,SID},PlayerData#player.type,data_carlos:get(mine_list)),
                          check_robot_act2({RID,SID},PlayerData,MineData,AiConfig) 
                  end, AiList).

check_robot_act2(RSID,PlayerData,MineData,[{ConditionType,NextLogic}|OtherAiNode])->
%%     ?INFO("check_robot_act2 -~w->>>>~w",[RSID,{ConditionType,NextLogic}]),
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
    StartTs = get(?pd_start_timestamp) + data_plane_ai:get(carlos_wait_time),
    Now =< StartTs;
check_condition(?condition_any,_RSID,_PlayerData,_MineData)->
    true;
%% 1)是不是死了
check_condition(?condition_alive,_RSID,PlayerData,_MineData)->
    Now = util:now(),
    Now =< PlayerData#player.rebornSec;
%% 2)判断是不是在岛屿外
check_condition(?condition_mine_outside,_RSID,PlayerData,MineData)->
    MineData =:= ?undefined orelse PlayerData#player.startPos /= PlayerData#player.endPos;
%% 3)所在岛屿是否是已经被己方占领的
check_condition(?condition_mine_self,_RSID,PlayerData,MineData)->
    Now = util:now(),
    MineData /= ?undefined andalso MineData#mine.owner == PlayerData#player.type andalso Now >= MineData#mine.beOwnSec;
%% 4)所在岛屿是否是正在被己方占领中
check_condition(?condition_mine_getting,_RSID,PlayerData,MineData)->
    Now = util:now(),
    MineData /= ?undefined andalso MineData#mine.owner == PlayerData#player.type andalso Now < MineData#mine.beOwnSec;
%% 5)所在岛屿中是否存在其他敌人
check_condition(?condition_mine_have_enemy,_RSID,PlayerData,MineData)->
    MineData /= ?undefined andalso 
        ((PlayerData#player.type =:= attacker andalso MineData#mine.defenderList /= []) orelse
         (PlayerData#player.type =:= defender andalso MineData#mine.attackerList /= []));
%% 6)所在岛屿中是否存在其他敌人
check_condition(?condition_mine_me_alone,_RSID,PlayerData,MineData)->
    case PlayerData#player.type of
        attacker when MineData /= ?undefined ->
            Num = erlang:length(MineData#mine.attackerList),
            Num =< 1;
        defender when MineData /= ?undefined ->
            Num = erlang:length(MineData#mine.defenderList),
            Num =< 1;
        true ->
            false
    end;
%% 7)是不是没移动
check_condition(?condition_no_move,_RSID,PlayerData,_MineData)->
    PlayerData#player.startPos =:= PlayerData#player.endPos;
%% 8)
check_condition(?condition_target_self,_RSID,PlayerData,_MineData)->
    Now = util:now(),
    TarMineData = war_server:get_mine(PlayerData#player.tarMineID),
    TarMineData /= ?undefined andalso TarMineData#mine.owner =:= PlayerData#player.type andalso TarMineData#mine.beOwnSec < Now;
%% 9)
check_condition(?condition_target_getting,_RSID,PlayerData,_MineData)->
    Now = util:now(),
    TarMineData = war_server:get_mine(PlayerData#player.tarMineID),
    TarMineData /= ?undefined andalso TarMineData#mine.owner =:= PlayerData#player.type andalso TarMineData#mine.beOwnSec >= Now.

do_act(?act_wait,_RSID,_PlayerData,_MineData)->
    ignore;
do_act(?act_ownMine,RSID,_PlayerData,MineData)->
    {RoleID,ServerID} = RSID,
    ?INFO("--ai--(~w)占领~w",[RSID,MineData#mine.id]),
    war_server:cs_carlos_ownMine(MineData#mine.id,ServerID,RoleID);
do_act(?act_move_type1,RSID,PlayerData,_MineData)->
    case get_can_attack_mine(PlayerData#player.type,data_carlos:get(mine_list),[]) of
        [] ->
            ignore;
        List ->
            AttackMineId = util:random_one_from_list(List),
            {RoleID,ServerID} = RSID,
            ?INFO("--ai--(~w)向~w岛移动",[RSID,AttackMineId]),
            CmdMsg = {{cs_carlos_mov, AttackMineId, true}, {?undefined,RoleID,ServerID}},
            erlang:send(self(), CmdMsg)
    end;
do_act(?act_attack_type1,RSID,PlayerData,MineData)->
    case MineData of
        ?undefined ->
            ignore;
        _ ->
            EnemyList = 
                case PlayerData#player.type of
                    attacker ->
                        MineData#mine.defenderList;
                    defender ->
                        MineData#mine.attackerList
                end,
            case EnemyList of
                [] ->
                    ignore;
                _ ->
                    {TarRoleID,TarServerID} = util:random_one_from_list(EnemyList),
                    {RoleID,ServerID} = RSID,
                    ?INFO("--ai--(~w)向~w发起攻击",[RSID,{TarRoleID,TarServerID}]),
                    CmdMsg = {{cs_carlos_attack, TarRoleID,TarServerID,MineData#mine.id,true}, {?undefined,RoleID,ServerID}},
                    erlang:send(self(), CmdMsg)
            end
    end.

show_all_battle_player()->
    [gen_server:call(PID, test_show_battle_player)||{_,PID,_,[war_server]}<-supervisor:which_children(war_sup)].

show_battle_info()->
    {[war_server:get_mine(MineID)||{MineID,_,_}<-data_carlos:get(mine_list)]
    ,show_battle_player()}.

show_battle_player()->
    AfkLimit = util:now() - data_carlos:get(afk_check_time),
    PlayerList = get(?pd_player_list),
    AfkPlayerList = 
        lists:filter(fun({RID,_SID})->
                        AfkTime = get({pd_operate_ts,RID}),
                        AfkTime =:= ?undefined orelse AfkTime < AfkLimit
                end, PlayerList),
    RobotList = get(?pd_robot_list),
    AiList = AfkPlayerList ++ RobotList,
    lists:map(fun({RID,SID})-> P=war_server:get_player(RID,SID),P#player{fighters=[],baseFighters=[]} end, AiList).

    
