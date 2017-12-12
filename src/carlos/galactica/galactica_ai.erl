-module(galactica_ai) .
-include("common.hrl").
-include("def_carlos.hrl").
-export([]).

-define(condition_any, 0).      %% 其他并列条件不符合，剩余默认处理的情况
-define(condition_alive, 1).    %% 是否还活着
-define(condition_mine_outside, 2). %% 是否在岛屿外
%% -define(condition_mine_self, 3).    %% 所在岛屿是否是己方占领的
%% -define(condition_mine_getting, 4). %% 岛屿是否在占领中
-define(condition_mine_have_enemy, 5).  %% 岛屿中是否存在其他敌人
%% -define(condition_mine_me_alone, 6).
-define(condition_no_move, 7).
%% -define(condition_mine_home, 8).
-define(condition_mine_full, 9).
-define(condition_not_start, 21).

-define(act_wait, 1001).
%% -define(act_ownMine, 1002).
-define(act_move_type1, 1011).
-define(act_attack_type1, 1021).

-define(pd_robot_list, pd_robot_list).
-define(pd_player_list, pd_player_list).        %% 用于寻找挂机者
-define(pd_start_timestamp, pd_start_timestamp).

init(PlayerListAll)->
    {PlayerList,RobotList} = 
        lists:foldl(fun(P,{AccPlayerList,AccRobotList})->
                if
                    P#ga_player.roleID > ((P#ga_player.serverID+1)*1000000) andalso 
                    P#ga_player.roleID < (((P#ga_player.serverID+1)*1000000)+10000) ->
                        put({pd_operate_ts,P#player.roleID},util:now()+3600),       %% 避免机器人显示挂机
                        {AccPlayerList,[{P#ga_player.roleID,P#ga_player.serverID}|AccRobotList]};
                    true->
                        put({pd_operate_ts,P#player.roleID},util:now()),       %% 避免机器人显示挂机
                        {[{P#ga_player.roleID,P#ga_player.serverID}|AccPlayerList],AccRobotList}
                end
            end, {[],[]}, PlayerListAll),
    ?INFO("PlayerList:~w  ++  RobotList:~w",[PlayerList,RobotList]),
    put(?pd_start_timestamp,util:now()),
    put(?pd_player_list,PlayerList),
    put(?pd_robot_list,RobotList).

get_arrived_mine({_RID,_SID},_Type,[])->
    ?undefined;
get_arrived_mine({RID,SID},Type,[Mine|OtherMineIdList])->
    {MineID,_,_,_,_,_,_} = Mine,
    MineData = galactica_war_server:get_mine(MineID),
    case galactica_war_server:check_mine_player(Type,{RID,SID},MineData) of
        true ->
            MineData;
        false ->
            get_arrived_mine({RID,SID},Type,OtherMineIdList)
    end.

get_can_attack_mine(_Type,[],_HomeMineIdList,AccList)->
    AccList;
get_can_attack_mine(Type,[{MineID,_,_,_,_,_,_}|OtherMineIdList],HomeMineIdList,AccList)->
    MineData = galactica_war_server:get_mine(MineID),
    case lists:member(MineID, HomeMineIdList) of
        false ->
            Num = 
                case Type of
                    attacker -> erlang:length(MineData#ga_mine.attackerList) ;
                    defender -> erlang:length(MineData#ga_mine.defenderList)
                end,
            if
                Num < MineData#ga_mine.maxNum ->
                    get_can_attack_mine(Type,OtherMineIdList,HomeMineIdList,[MineID|AccList]);
                true ->
                    get_can_attack_mine(Type,OtherMineIdList,HomeMineIdList,AccList)
            end;
        true ->
            get_can_attack_mine(Type,OtherMineIdList,HomeMineIdList,AccList)
    end.

check_robot_act()->
    AfkLimit = util:now() - data_galactica:get(afk_check_time),
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
                                                          ,{?condition_mine_full,?act_move_type1}
                                                          ,{?condition_any,?act_wait}]}
                                ,{?condition_any,[{?condition_mine_have_enemy,?act_attack_type1}
                                                 ,{?condition_any,?act_wait}]}]}],
    lists:foreach(fun({RID,SID})-> 
                          PlayerData = galactica_war_server:get_player(RID,SID),
                          MineData = get_arrived_mine({RID,SID},PlayerData#ga_player.type,data_galactica:get(mine_list)),
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
    StartTs = get(?pd_start_timestamp) + data_plane_ai:get(galactica_wait_time),
    Now =< StartTs;
check_condition(?condition_any,_RSID,_PlayerData,_MineData)->
    true;
%% 1)是不是死了
check_condition(?condition_alive,_RSID,PlayerData,MineData)->
    Now = util:now(),
    Now =< PlayerData#ga_player.rebornSec;
%% 2)判断是不是在岛屿外
check_condition(?condition_mine_outside,_RSID,PlayerData,MineData)->
    MineData =:= ?undefined;
%% %% 3)所在岛屿是否是已经被己方占领的
%% check_condition(?condition_mine_self,_RSID,PlayerData,MineData)->
%%     Now = util:now(),
%%     MineData /= ?undefined andalso MineData#ga_mine.owner =:= PlayerData#ga_player.type andalso Now >= MineData#ga_mine.beOwnSec;
%% 4)所在岛屿是否是正在被己方占领中
%% check_condition(?condition_mine_getting,_RSID,PlayerData,MineData)->
%%     Now = util:now(),
%%     MineData /= ?undefined andalso MineData#ga_mine.beOwnSec =:= PlayerData#ga_player.type andalso Now < MineData#ga_mine.beOwnSec;
%% 5)所在岛屿中是否存在其他敌人
check_condition(?condition_mine_have_enemy,_RSID,PlayerData,MineData)->
    MineData /= ?undefined andalso 
        ((PlayerData#ga_player.type =:= attacker andalso MineData#ga_mine.defenderList /= []) orelse
         (PlayerData#ga_player.type =:= defender andalso MineData#ga_mine.attackerList /= []));
%% %% 6)所在岛屿中是否存在其他敌人
%% check_condition(?condition_mine_me_alone,_RSID,PlayerData,MineData)->
%%     case PlayerData#ga_player.type of
%%         attacker when MineData /= ?undefined ->
%%             Num = erlang:length(MineData#ga_mine.attackerList),
%%             Num =< 1;
%%         defender when MineData /= ?undefined ->
%%             Num = erlang:length(MineData#ga_mine.defenderList),
%%             Num =< 1;
%%         true ->
%%             false
%%     end;
%% 7)是不是没移动
check_condition(?condition_no_move,_RSID,PlayerData,MineData)->
    PlayerData#ga_player.tarMineID =:= 0;
%% 7)是不是没移动
%% check_condition(?condition_mine_home,_RSID,PlayerData,MineData)->
%%     PlayerData#ga_player.startPos =:= PlayerData#ga_player.endPos.
check_condition(?condition_mine_full,_RSID,PlayerData,_MineData)->
    Now = util:now(),
    TarMineData = galactica_war_server:get_mine(PlayerData#ga_player.tarMineID),
    if
        TarMineData /= ?undefined ->
            Num = 
                case PlayerData#ga_player.type of
                    attacker ->
                        erlang:length(TarMineData#ga_mine.attackerList);
                    defender ->
                        erlang:length(TarMineData#ga_mine.defenderList)
                end,
            Num >= TarMineData#ga_mine.maxNum;
        true ->
            false
    end.

do_act(?act_wait,RSID,PlayerData,MineData)->
    ignore;
%% do_act(?act_ownMine,RSID,PlayerData,MineData)->
%%     {RoleID,ServerID} = RSID,
%%     ?INFO("--ai--(~w)占领~w",[RSID,MineData#ga_mine.id]),
%%     galactica_war_server:cs_carlos_ownMine(MineData#ga_mine.id,RoleID,ServerID);
do_act(?act_move_type1,RSID,PlayerData,MineData)->
    HomeList = [get({home, I})||{I,_}<-data_galactica:get({home_list,attacker}) ++ data_galactica:get({home_list,defender})],
    HomeMineIdList = lists:merge([H#ga_home.mineIDs||H<-HomeList]),
    case get_can_attack_mine(PlayerData#ga_player.type,data_galactica:get(mine_list),HomeMineIdList,[]) of
        [] ->
            ignore;
        List ->
            AttackMineId = util:random_one_from_list(List),
            {RoleID,ServerID} = RSID,
            ?INFO("--ai--(~w)向~w岛移动",[RSID,AttackMineId]),
            CmdMsg = {{cs_galactica_mov, AttackMineId, true}, {?undefined,RoleID,ServerID}},
            erlang:send(self(), CmdMsg)
    end;    
do_act(?act_attack_type1,RSID,PlayerData,MineData)->
    case MineData of
        ?undefined ->
            ignore;
        _ ->
            EnemyList = 
                case PlayerData#ga_player.type of
                    attacker ->
                        MineData#ga_mine.defenderList;
                    defender ->
                        MineData#ga_mine.attackerList
                end,
            case EnemyList of
                [] ->
                    ignore;
                _ ->
                    {TarRoleID,TarServerID} = util:random_one_from_list(EnemyList),
                    {RoleID,ServerID} = RSID,
                    ?INFO("--ai--(~w)向~w发起攻击",[RSID,{TarRoleID,TarServerID}]),
                    CmdMsg = {{cs_galactica_attack, TarRoleID,TarServerID,MineData#ga_mine.id,true}, {?undefined,RoleID,ServerID}},
                    erlang:send(self(), CmdMsg)
            end
    end.
    