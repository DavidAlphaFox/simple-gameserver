-module(carlos_aux).

-compile(export_all).
-include("common.hrl").
-include("def_carlos.hrl").
-include("all_proto.hrl").

parse_rq2members(RequestList) ->
    loop_requestList(RequestList, fun parse_rq2members2/4, [], [], 1).

parse_rq2members2(RequestList, TeamList) ->
    loop_requestList(RequestList, fun parse_rq2members2/4, TeamList, [], 1).

parse_rq2members2(Team, TeamList, MemberList, Camp) ->
    lists:foldl(fun(Request, Acc) ->
                    case Request of 
                        #request{id=TeamID, members=Info} ->
                            next;
                        #tw_request{id=TeamID, members=Info} ->
                            next
                    end,
                    lists:foldl(fun(MemberInfo, {TeamAcc, MemberAcc}) ->
                                    case MemberInfo of
                                        #member_base_info{serverID=ServerIDT, roleID=RoleID, head=Head, isMale=IsMale, title=Title, level=Level, fight_power=FightPower} ->
                                            next;
                                        %% 因为match server会缓存数据,所以这里会收到旧的member_base_info,然后会导致格式不匹配,就挂了
                                        {_, ServerIDT, RoleID} ->
                                            Head = 0,
                                            IsMale = 0,
                                            Title = 0,
                                            Level = 45,
                                            FightPower = 0
                                    end,
                                    Member = #room_member{serverID=ServerIDT, roleID=RoleID, teamID=TeamID, camp=Camp, head=Head, isMale=IsMale, title=Title, level=Level, fight_power=FightPower},
                                    MemberAcc2 = [Member|MemberAcc],
                                    TeamAcc2 = 
                                        case lists:keytake(TeamID, 1, TeamAcc) of 
                                            false ->
                                                [{TeamID, [RoleID]}|TeamAcc];
                                            {value, {_, NowList}, RestList} ->
                                                [{TeamID, [RoleID|NowList]}|RestList]
                                        end,
                                    {TeamAcc2, MemberAcc2}
                                end, Acc, Info) 
                end, {TeamList, MemberList}, Team).

loop_requestList([], _, TeamAcc, MemberAcc, _) ->
    {TeamAcc, MemberAcc};

loop_requestList([H|T], Fun, TeamAcc, MemberAcc, Camp) ->
    {TeamAcc2, MemberAcc2} = Fun(H, TeamAcc, MemberAcc, Camp),
    loop_requestList(T, Fun, TeamAcc2, MemberAcc2, Camp + 1).

find_match_lvl(MemberList, Mod, High, Low) ->
    Info = calc_avg_lvl(MemberList),
    find_match_lvl2(Info, Mod, High, Low).

find_match_lvl2(_, _, Low, Low) ->
    Low;

find_match_lvl2({Lvl, FightPower}=Info, Mod, High, Low) ->
    {NeedLvl, NeedFP, _} = erlang:apply(Mod, get, [{fight_level, High}]),
    case Lvl >= NeedLvl andalso FightPower >= NeedFP of
        true ->
            High;
        _ ->
            find_match_lvl2(Info, Mod, High - 1, Low) 
    end.

calc_avg_lvl(MemberList) ->
    {NumT, LvlT, FPT} =
        lists:foldl(fun(#room_member{camp=Camp, level=Level, fight_power=FP}, {NumAcc, LvlAcc, FPAcc} = Acc) ->
                        %% 只统计一组的 
                        case Camp =:= 1 of
                            true ->
                                {NumAcc + 1, LvlAcc + Level, FPAcc + FP};
                            _ ->
                                Acc
                        end
                    end, {0, 0, 0}, MemberList),
    {erlang:round(LvlT / NumT), erlang:round(FPT / NumT)}.
                            
%% 这里的时间间隔使用进程字典来处理,主要是因为大部分玩家都是正常操作,如果用ets来共享数据,
%% 在role_server来进行判断,因为ets并发读写效率可能会有影响,而且目前飞机用的ets都没有打开并发
%% 读写选项,所以会影响正常操作的效率.这儿在world层用进程字典来处理,相互区分来不同的飞机模块,
%% 同时对效率的影响也不会太大,因为异常操作的是少数,因为这儿导致报名失败的机率很小.
get_last_unrequest_time(RoleID) ->
    case erlang:get({last_request_time, RoleID}) of
        undefined ->
            0;
        Any ->
            Any
    end.

check_team_sign_time(IDList) ->
    Now = util:now(),
    UIS = data_carlos:get(unrequest_inteval_second),
    lists:all(fun(E) ->
                Now - get_last_unrequest_time(E) > UIS
            end, IDList).
                   
set_team_unrequest_time(IDList) ->
    Now = util:now(),
    lists:foreach(fun(E) -> erlang:put({last_request_time, E}, Now)  end, IDList).
