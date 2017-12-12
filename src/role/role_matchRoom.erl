-module(role_matchRoom).
-compile(export_all).
-include("def_role.hrl").
-include("def_reward.hrl").
-include("def_carlos.hrl").

%% API functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_matchRoom_ready(#cs_matchRoom_ready{roomID=RoomID}) ->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    erlang:send(match_room_server, {client_msg, ready, RoomID, RoleID}).

cs_matchRoom_cancel(#cs_matchRoom_cancel{roomID=RoomID}) ->
    #role{roleID=RoleID} = role_data:get_roleInfo(),
    erlang:send(match_room_server, {client_msg, cancel, RoomID, RoleID}).
    
cs_matchRoom_exit(#cs_matchRoom_exit{roomID=RoomID}) ->
    #role{roleID=RoleID, teamId=TeamID} = role_data:get_roleInfo(),
    SignID = 
        case TeamID =:= -1 of
            true ->
                ?make_id(-RoleID, 1);
            _ ->
                case ets:lookup(ets_team_list, TeamID) of
                    [#team_info{team_member=TeamMember}] ->
                        ?make_id(TeamID, erlang:length(TeamMember));
                    _ ->
                       0 
                end 
        end,
    case SignID of
        0 ->
            ?ERR("cs_matchRoom_exit SignID get zero,RoleID:~p,TeamID:~p.~n", [RoleID, TeamID]);
        _ ->
            %% 客户端有可能会发无效的roomid过来,然后这边取出来就是卡洛斯的房间类型,会导致异常退出卡洛斯匹配
            %% cancel没用,ready准备了也无所谓
            case RoomID of
                ?invalid_room_id ->
                    ?ERR("cs_matchRoom_exit role:~p, get invalid room id~n", [RoleID]);
                _ ->
                    erlang:send(match_room_server, {client_msg, exit, RoomID, SignID}) 
            end
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================

