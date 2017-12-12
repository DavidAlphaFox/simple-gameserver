-module(role_lvlSgAttr).
-include("def_role.hrl").

-compile(export_all).

-define(LVLSG_UP_TYPE_ROLE, 0).
-define(LVLSG_UP_TYPE_GER_LVL, 1).
-define(LVLSG_UP_TYPE_EQUIP_LVL, 2).
-define(LVLSG_UP_TYPE_GER_RANK, 3).
-define(LVLSG_UP_TYPE_EQUIP_RANK, 4).
-define(LVLSG_UP_TYPE_EQUIP_CHANGE, 5).

on_role_lvl_up(OldLvl, NewLvl) ->
    MKK = {data_lvl_attr_role, lvl_interval, segment, fun get_add_attr/4},
    on_lvl_up(?LVLSG_UP_TYPE_ROLE, MKK, 0, OldLvl, 0, NewLvl).

on_ger_lvl_up(Star, OldLvl, NewLvl) ->
    MKK = {data_lvl_attr_ger, lvl_interval, segment, fun get_add_attr2/4},
    on_lvl_up(?LVLSG_UP_TYPE_GER_LVL, MKK, Star, OldLvl, Star, NewLvl).

on_ger_rank_up(Star, OldRank, NewRank) ->
    MKK = {data_lvl_attr_ger, rank_interval, rank_segment, fun get_add_attr2/4},
    on_lvl_up(?LVLSG_UP_TYPE_GER_RANK, MKK, Star, OldRank, Star, NewRank).

%% 因为一个精灵身上的全部装备算一个整体,所以这儿只做整体判断
on_equip_lvl_up(GerID, OldEquipList) ->
    MKK = {data_lvl_attr_equip, lvl_interval, segment, fun get_add_attr2/4},
    on_euqip_lvlsg_change(GerID, ?LVLSG_UP_TYPE_EQUIP_LVL, MKK, #item.itemLevel, OldEquipList).

on_equip_rank_up(GerID, OldEquipList) ->
    MKK = {data_lvl_attr_equip, rank_interval, rank_segment, fun get_add_attr2/4},
    on_euqip_lvlsg_change(GerID, ?LVLSG_UP_TYPE_EQUIP_RANK, MKK, #item.itemRank, OldEquipList).

on_equip_change(GerID, OldEquipListT) ->
    OldEquipList = filter(OldEquipListT),
    EquipList = filter(role_data:get_equip(GerID)),
    OldAddAttr = cacl_equip_lvlsg_add(OldEquipList),
    NewAddAttr = cacl_equip_lvlsg_add(EquipList),
    Attr = ger_attr:append_add_attr(NewAddAttr, setelement(1, OldAddAttr, sub_attr), true),
    case Attr =:= #add_attr{} of
        true ->
            ignored;
        _ ->
            PAttr = setelement(1, Attr, p_lvlSgAttr_attr),
            ?sendself(#sc_lvlSgAttr_inc{type=?LVLSG_UP_TYPE_EQUIP_CHANGE, inc=0, attr=PAttr}) 
    end.

on_euqip_lvlsg_change(GerID, Type, MKK, Property, OldEquipListT) ->
    OldEquipList = filter(OldEquipListT),
    %%?ERR("on_euqip_lvlsg_change,GerID:~p~nType:~p~nMKK:~p~nProperty:~p~n", [GerID, Type, MKK, Property]),
    EquipList = filter(role_data:get_equip(GerID)),
    %%?ERR("Old:~p~nNew:~p~n", [OldEquipList, EquipList]),
    case check_euqip_lvlsg(EquipList, Property) of
        false ->
            ignored;
        {ok, Star, Level} ->
            case check_euqip_lvlsg(OldEquipList, Property) of
                false ->
                    ignored;
                {ok, OStar, OldLvl} ->
                    %%?ERR("Star:~p~nLevel:~p~nOStar:~p~nOldLvl:~p~n", [Star, Level, OStar, OldLvl]),
                    on_lvl_up(Type, MKK, OStar, OldLvl, Star, Level)
            end
    end.

on_lvl_up(Type, {Mod, KI, KA, SF}, OldStar, OldLvl, NewStar, NewLvl) ->
    Interval = erlang:apply(Mod, get, [KI]),
    SegmentO = erlang:trunc(OldLvl / Interval),
    SegmentN = erlang:trunc(NewLvl / Interval),
    %%?ERR("Type:~p,OldStar:~p,OldLvl:~p,NewStar:~p,NewLvl:~p,SegmentO:~p,SegmentN:~p.~n", [Type, OldStar, OldLvl, NewStar, NewLvl, SegmentO, SegmentN]),
    case SegmentN =:= SegmentO of
        true ->
            ignored;
        _ ->
            %% 因为角色升级是不会刷新精灵列表的,所以这儿需要主动刷新下
            case Type =:= ?LVLSG_UP_TYPE_ROLE of
                true ->
                    role_data:mark_role_lvl_up_refresh(); 
                _ ->
                    ignored
            end,
%%             case SegmentN > SegmentO of
%%                 true ->
%%                     notice_client(Type, {Mod, KI, KA, SF}, OldStar, NewStar, SegmentO, SegmentN, 1);
%%                 _ ->
%%                     notice_client(Type, {Mod, KI, KA, SF}, OldStar, NewStar, SegmentO, SegmentN, -1)
%%             end
            OldAddAttr = SF(Mod, KA, OldStar, SegmentO),
            NewAddAttr = SF(Mod, KA, NewStar, SegmentN),
            IncAttr = ger_attr:append_add_attr(NewAddAttr, setelement(1, OldAddAttr, sub_attr), true),
            PAttr = erlang:setelement(1, IncAttr, p_lvlSgAttr_attr),
            %?ERR("OldAddAttr:~p~n NewAddAttr:~p~n", [OldAddAttr, NewAddAttr]),
            ?sendself(#sc_lvlSgAttr_inc{type=Type, inc=NewLvl - OldLvl, attr=PAttr})
    end.

notice_client(Type, {Mod, KI, KA, SF}, OldStar, NewStar, Start, End, Interval) ->
    case Start =:= End of
        true ->
            ignored;
        _ ->
            OldAddAttr = SF(Mod, KA, OldStar, Start),
            NewAddAttr = SF(Mod, KA, NewStar, Start+Interval),
            IncAttr = ger_attr:append_add_attr(NewAddAttr, setelement(1, OldAddAttr, sub_attr), true),
            PAttr = erlang:setelement(1, IncAttr, p_lvlSgAttr_attr),
            ?sendself(#sc_lvlSgAttr_inc{type=Type, inc=Interval, attr=PAttr}),
            notice_client(Type, {Mod, KI, KA, SF}, OldStar, NewStar, Start + Interval, End, Interval)
    end.

%% 获得精灵的等级段加成,包括等级加成和品阶加成
calc_ger_lvlsg_add(#ger{gerBase=#gerBase{gerTypeID=GerTypeID, gerQuality=GerRank, gerLevel=GerLevel}}) ->
    calc_ger_lvlsg_add2(GerTypeID, GerRank, GerLevel);

calc_ger_lvlsg_add(#gerSimple{gerTypeID=GerTypeID, gerQuality=GerRank, gerLevel=GerLevel}) ->
    calc_ger_lvlsg_add2(GerTypeID, GerRank, GerLevel).
    
calc_ger_lvlsg_add2(GerTypeID, GerRank, GerLevel) ->
    %?ERR("calc_ger_lvlsg_add2:~p,~p,~p.~n", [GerTypeID, GerRank, GerLevel]),
    #data_ger{gerStar=GerStar} = data_ger:get(GerTypeID),
    Interval = data_lvl_attr_ger:get(lvl_interval),
    Segment = erlang:trunc(GerLevel / Interval),
    AddAttr = 
        case data_lvl_attr_ger:get({segment, GerStar, Segment}) of
            Any when is_record(Any, add_attr) ->
                Any;
            _ ->
                0
        end,
    RankI = data_lvl_attr_ger:get(rank_interval),
    RSegment = erlang:trunc(GerRank / RankI), 
    case data_lvl_attr_ger:get({rank_segment, GerStar, RSegment}) of
        RAny when is_record(RAny, add_attr) ->
            ger_attr:append_add_attr(AddAttr, RAny);
        _ ->
            AddAttr
    end.
    
%% 获得玩家的等级段加成
calc_role_lvlsg_add() ->
    case role_data:get_roleInfo() of
        #role{level=Level} ->
            %?ERR("calc_ger_lvlsg_add:~p.~n", [Level]),
            Interval = data_lvl_attr_role:get(lvl_interval),
            Segment = erlang:trunc(Level / Interval),
            case data_lvl_attr_role:get({segment, Segment}) of
                Any when is_record(Any, add_attr) ->
                    Any;
                _ ->
                    0
            end;
        _ ->
            0
    end.

%% 获得装备的等级段加成,包括装备等级和装备精炼两部分
cacl_equip_lvlsg_add(EquipList) ->
    %?ERR("calc_ger_lvlsg_add:~w, ~p~n", [EquipList, check_euqip_lvlsg(EquipList)]), 
    case check_euqip_lvlsg(EquipList) of
        false ->
            #add_attr{};
        {ok, Star, MinLvl, MinRank} ->
            %?ERR("calc_ger_lvlsg_add,Star:~p~nMinLvl:~p~nMinRank:~p~n", [Star, MinLvl, MinRank]),
            Interval = data_lvl_attr_equip:get(lvl_interval),
            Segment = erlang:trunc(MinLvl / Interval), 
            AddAttr = 
                case data_lvl_attr_equip:get({segment, Star, Segment}) of
                    Any when is_record(Any, add_attr) ->
                        %?ERR("lvl,Interval:~p~nSegment:~p~nAdd:~p~n", [Interval, Segment, Any]),
                        Any;
                    _ ->
                        #add_attr{}
                end,

            RankI = data_lvl_attr_equip:get(rank_interval),
			MinRank2 = if MinRank > 10 -> 10; true -> MinRank end,
            RSegment = erlang:trunc(MinRank2 / RankI),
            case data_lvl_attr_equip:get({rank_segment, Star, RSegment}) of
                RAny when is_record(RAny, add_attr) ->
                    %?ERR("rank,Interval:~p~nSegment:~p~nAdd:~p~n", [RankI, RSegment, RAny]),
                    ger_attr:append_add_attr(AddAttr, RAny);
                _ ->
                    AddAttr
            end
    end.

check_euqip_lvlsg(EquipListT) ->
    EquipList = filter(EquipListT),
    Len = erlang:length(EquipList),
    case Len < data_lvl_attr_equip:get(need_number) of
        true ->
            false;
        _ ->
            case check_equip_star(EquipList) of
                false ->
                    false;
                {ok, Star} ->
                    {MinLvl, _} = util:keymin(EquipList, #item.itemLevel),
                    {MinRank, _} = util:keymin(EquipList, #item.itemRank),
                    {ok, Star, MinLvl, MinRank}
            end
    end.

check_euqip_lvlsg(EquipList, Property) ->
    Len = erlang:length(EquipList),
    case Len < data_lvl_attr_equip:get(need_number) of
        true ->
            false;
        _ ->
            case check_equip_star(EquipList) of
                false ->
                    false;
                {ok, Star} ->
                    {MinPropertyV, _} = util:keymin(EquipList, Property),
                    {ok, Star, MinPropertyV}
            end
    end.

check_equip_star(EquipList) -> 
    GetFunc = fun(#item{itemTypeID=ItemTypeID}) -> 
                #data_item{itemStar=ItemStar} = data_item:get(ItemTypeID),
                ItemStar
             end,
    check_equip_same_property(EquipList, GetFunc).

check_equip_same_property([H|T], GetFunc) -> 
    PropertyV = GetFunc(H), 
    check_equip_same_property(T, GetFunc, PropertyV).

check_equip_same_property([], _, PropertyV) ->
    {ok, PropertyV};

check_equip_same_property([H|T], GetFunc, PropertyV) ->
    PropertyVT = GetFunc(H),
    case PropertyVT =:= PropertyV of
        true ->
            check_equip_same_property(T, GetFunc, PropertyV);
        _ ->
            false
    end.

get_add_attr(M, K, _Star, Level) ->
    case erlang:apply(M, get, [{K, Level}]) of
        Any when is_record(Any, add_attr) ->
            Any;
        _ ->
            #add_attr{}
    end.


get_add_attr2(M, K, Star, Level) ->
    case erlang:apply(M, get, [{K, Star, Level}]) of
        Any when is_record(Any, add_attr) ->
            Any;
        _ ->
            #add_attr{}
    end.
    
filter(EquipList) ->
    lists:filter(fun(#item{itemPos=ItemPos}) -> ItemPos > 0 andalso ItemPos =< 6 end, EquipList).
