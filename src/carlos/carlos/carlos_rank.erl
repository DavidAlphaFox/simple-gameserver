-module(carlos_rank).
-compile(export_all).
-include("def_carlos.hrl").
-include("def_mail.hrl").
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-define(CURRENT_SEASON_INFO,current_season_info).
-define(PRE_SEASON_INFO,pre_season_info).
-define(PersistList,persist_list).
-define(MAX_NUM,999999999).
-define(EQUAL,equal).
-define(ATTACKER,attacker).
-define(DEFENDER,defender).
-define(CURRENTSEASON,1).
-define(PRESEASON,2).
-define(CARLOS_REWARD_DELAY,60*60).
-define(PLAYER_CHANGE,1).
-define(PLAYER_NO_CHANGE,0).
-define(DUMP_INTERVAL,300).
-record(state, {}).
start( ) ->
    {ok,_} = 
    supervisor:start_child(carlos_sup,
                            {?MODULE,
                             {?MODULE, start_link,[]},
                             permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case data_setting:get(server_type) of
        carlos_match ->
            process_flag(trap_exit,true),
            init_rank_info(),
            erlang:send_after(?DUMP_INTERVAL*1000,self(),dump_data),
            erlang:send_after(?DUMP_INTERVAL*1000,self(),do_hibernate),
            {ok, #state{}};
        _->
            ?ERR("not carlos match server"),
            ignore
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info(do_hibernate,State)->
    erlang:send_after(?DUMP_INTERVAL*1000, self(),do_hibernate),
    erlang:garbage_collect(),
    {noreply, State,hibernate};    
handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    do_persist(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_handle_info({update_rank_info,Result,P_Player_Info_List},State)->
    carlos_rank:update_rank_info(Result,P_Player_Info_List),
    {noreply,State};

do_handle_info({syn_rankinfo,Type,Force},State)->
    carlos_rank:syn_rankinfo(Type,Force),
    {noreply,State};

do_handle_info({resortrankinfo,Type},State)->
    carlos_rank:resort_rankinfo(Type),
    {noreply,State};

do_handle_info(new_season,State)->
    carlos_rank:begin_new_season(),
    {noreply,State};

do_handle_info({get_rankinfo,ServerID,Type},State)->
    carlos_rank:syn_rankinfo_server(ServerID,Type),
    {noreply,State};

do_handle_info({syn_rankinfo,Force},State)->
    carlos_rank:syn_rankinfo(Force),
    {noreply,State};

do_handle_info({erase,Name},State)->
    carlos_rank:erase(Name),
    {noreply,State};
do_handle_info({deduct_carlostime,Msg},State)->
    carlos_rank:deduct_carlostime(Msg),
    {noreply,State};

do_handle_info({fix_role_carlosinfo,Player},State)->
    carlos_rank:update_player_info({false,Player}),
    {noreply,State};

do_handle_info({send_season_reward,Type},State)->
    SeasonInfo = get_seasoninfo(Type),
    send_season_reward(SeasonInfo),
    {noreply,State};

do_handle_info(fix_p_player,State)->
    do_fix_p_player(),
    {noreply,State};

do_handle_info({fix_seasoninfo,SeasonID,Type},State)->
    RealType = case Type of
        1->
            ?CURRENT_SEASON_INFO;
        2->
            ?PRE_SEASON_INFO
    end,
    fix_set_seasoninfo(SeasonID,RealType),
    {noreply,State};

do_handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
do_handle_info(dump_data,State)->
    erlang:send_after(?DUMP_INTERVAL*1000,self(),dump_data),
    do_persist(),
    {noreply,State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

% -record(season_info,{seasonid=0,year=0,type=0,rankinfolist=[]}).
% -record(rank_seg,{id=0,length=0,minscore=0,maxscore=0,playerlist=[]}).
init_rank_info()->
    AllSeasonInfo = case db_sql:get_etc(?DB_ETC_KEY_CARLOS_RANK) of
        []->
            init_rank_info2();
        X when is_list(X)->
            [fix_seasoninfo(E)||E<-X];
        _ ->
            init_rank_info2()
    end,
    set_seasoninfo(get_seasoninfo_by_type(?CURRENTSEASON,AllSeasonInfo),?CURRENT_SEASON_INFO),
    set_seasoninfo(get_seasoninfo_by_type(?PRESEASON,AllSeasonInfo),?PRE_SEASON_INFO),
    init_syn_rankinfo(),
    init_season_end().

init_season_end()->
    case data_carlos:get(season_test) of
        false->
            {Year,Mon,Day} = get_next_season_begin(erlang:date()),
            TimeStamp = util:datetime_to_seconds({{Year,Mon,Day},{0,0,1}}),
            Interval = TimeStamp - util:now(),
            ?INFO("非测试模式，下赛季距离现在：~w (S) ~n",[Interval]);
        true->
            Interval = data_carlos:get(season_interval)
    end,
    erlang:send_after(Interval*1000,?MODULE,new_season).

init_rank_info2()->
    {Year,_Mon,_Day} = erlang:date(),
    [#season_info{seasonid=1,year=Year,type=?CURRENT_SEASON_INFO,rankinfolist=init_rank_info3()},#season_info{year=Year,type=?PRE_SEASON_INFO,rankinfolist=init_rank_info3()}].

fix_set_seasoninfo(SeasonID,Type)->
    {Year,_Mon,_Day} = erlang:date(),
    SeasonInfo = #season_info{seasonid=SeasonID,year=Year,type=Type,rankinfolist=init_rank_info3()},
    set_seasoninfo(SeasonInfo,Type).

init_rank_info3()->
    Max_Store_rank_num = data_carlos:get(max_store_rank_num),
    Max_Seg_Length = data_carlos:get(max_seg_length),
    SegNum = get_seg_num(Max_Store_rank_num,Max_Seg_Length),
    [#rank_seg{id=ID,length=0,min=0,max=0}||ID<-lists:seq(1,SegNum)].


init_syn_rankinfo()->
    % SynInterval = data_carlos:get(syn_rankinfo_interval),
    carlos_rank:send_to_me({syn_rankinfo,false}).


syn_rankinfo(Force)->
    do_syn_rankinfo(?PRESEASON,Force),
    do_syn_rankinfo(?CURRENTSEASON,Force),
    erlang:send_after(get_next_syn_interval(), self(), {syn_rankinfo,false}).

syn_rankinfo(Type,Force)->
    do_syn_rankinfo(Type,Force).

%%改实现已经从内部实现了所有数据的全部同步
syn_rankinfo_server(ServerID,Type)->
    RankInfoList = get_rank_info_list(Type),
    NewRankInfoList = update_rank(RankInfoList),
    set_rank_info_list(Type,balance_seg(NewRankInfoList,Type)),
    AllPlayerList = lists:foldl(fun(Seg,Acc)->
        #rank_seg{playerlist=PlayerList} = Seg,
        Acc++PlayerList
    end,[],get_rank_info_list(Type)),
    SeasonInfo = get_seasoninfo(Type),
    send_msg:direct(ServerID,carlos_server,{update_rank_info,{lists:sublist(AllPlayerList,data_carlos:get(max_rank_num)),?generate_season_id(SeasonInfo#season_info.year,SeasonInfo#season_info.seasonid),Type,true}}).
    
balance_seg(RankInfoList,_Type)->
    %?INFO("开始重新平衡~n"),
    PlayerList = lists:foldl(fun(Seg,Acc)->
        Seg#rank_seg.playerlist ++ Acc
    end,[],RankInfoList),
    Result = lists:foldl(fun(Seg,Acc)->
        case get_seg_ranklist(Seg#rank_seg.id,PlayerList) of
            []->
                [Seg#rank_seg{playerlist=[],min=0,max=0,length=0}|Acc];
            NewPlayerList->
                [Seg#rank_seg{playerlist=NewPlayerList,min=lists:nth(length(NewPlayerList),NewPlayerList),max=hd(NewPlayerList),length=length(NewPlayerList)}|Acc]
        end
    end,[],RankInfoList), 
    Result.

clear_ischange_flage(Seg)->
    PlayerList = Seg#rank_seg.playerlist,
    NewPlayerList = [Player#p_player_info{ischange=?PLAYER_NO_CHANGE}||Player<-PlayerList],
    Seg#rank_seg{playerlist=NewPlayerList}.

get_seg_ranklist(SegID,PlayerList)->
    AllLength = length(PlayerList),
    ShowNum = data_carlos:get(max_seg_length),
    Begin = (SegID-1)*ShowNum+1,
    End = SegID * ShowNum,
    if
        Begin =< AllLength andalso End =< AllLength ->
            lists:sublist(PlayerList,Begin,ShowNum);
        Begin =< AllLength andalso End > AllLength->
            lists:sublist(PlayerList,Begin,AllLength-Begin+1);
        true->
            []
    end.
do_syn_rankinfo(Type)->
    do_syn_rankinfo(Type,false).
do_syn_rankinfo(Type,Force)->
    resort_rankinfo(Type),
    RankInfoList = get_rank_info_list(Type),
    NewRankInfoList = update_rank(RankInfoList),
    NewRankInfoList1 = balance_seg(NewRankInfoList,Type),
    NewRankInfoList2 = [clear_ischange_flage(Seg)||Seg<-NewRankInfoList1],
    set_rank_info_list(Type,NewRankInfoList2),
    AllPlayerList = lists:foldl(fun(Seg,Acc)->
        #rank_seg{playerlist=PlayerList} = Seg,
        SynPlayerList=lists:foldr(fun(Player,SynAcc)->
            case Player#p_player_info.ischange of
                ?PLAYER_CHANGE->
                    [Player|SynAcc];
                ?PLAYER_NO_CHANGE->
                    case Force of
                        false->
                            SynAcc;
                        true->
                            [Player|SynAcc]
                    end;
                _ ->
                    ?INFO("undefined ischange value:~w ~n",[Player#p_player_info.ischange]),
                    case Force of
                        false->
                            SynAcc;
                        true->
                            [Player|SynAcc]
                    end
            end
        end,[],PlayerList),
        Acc++SynPlayerList
    end,[],NewRankInfoList1),
    ?INFO("同步的数据：~w~n Length:~w ~n",[AllPlayerList,length(AllPlayerList)]),
    SeasonInfo = get_seasoninfo(Type),
    send_msg:broadcast(carlos_server,{update_rank_info,{AllPlayerList,?generate_season_id(SeasonInfo#season_info.year,SeasonInfo#season_info.seasonid),Type,Force}}). 

do_persist()->
    PreSeasonInfo = get_seasoninfo(?PRE_SEASON_INFO),
    CurrentSeasonInfo = get_seasoninfo(?CURRENT_SEASON_INFO),
    SeasonInfo = [PreSeasonInfo,CurrentSeasonInfo],
    ?INFO("持续化赛季信息：~w ~n",[SeasonInfo]),
    db_sql:set_etc(?DB_ETC_KEY_CARLOS_RANK,SeasonInfo).

update_rank(RankInfoList)->
    LengthList = [{Seg#rank_seg.id,get_rank_sum_by_id(Seg#rank_seg.id,RankInfoList)}||Seg<-RankInfoList],
    lists:foldl(fun(Seg,Acc)->
            Base = case lists:keyfind(Seg#rank_seg.id,1,LengthList) of
                false->
                    ?ERR("查找基础rank值未发现对应id：~w  LengthList:~w ~n",[Seg#rank_seg.id,LengthList]),
                    0;
                {_ID,Value}->
                    Value
            end,
            [update_seg_rank(Seg,Base)|Acc]
        end,[],RankInfoList).

get_rank_sum_by_id(ID,RankInfoList)->
    BeforeList = [Seg||Seg<-RankInfoList,Seg#rank_seg.id<ID],
    lists:foldl(fun(Seg,Acc)->
        Acc+Seg#rank_seg.length
    end,0,BeforeList).

update_seg_rank(Seg,Base)->
    {_,NewPlayerList} = lists:foldl(fun(Player,{Acc,PlayerList})->
        {Acc+1,[Player#p_player_info{rank=Base+Acc+1}|PlayerList]}
    end,{0,[]},Seg#rank_seg.playerlist),
    % ?INFO("NewPlayerList:~w ~n",[NewPlayerList]),
    Seg#rank_seg{playerlist=lists:reverse(NewPlayerList)}.

resort_rankinfo(Type)->
    RankInfoList = get_rank_info_list(Type),
    NewRankInfoList = lists:foldl(fun(Seg,Acc)->
        #rank_seg{playerlist=PlayerList} = Seg,
        case length(PlayerList) > 0 of
            true->
                NewPlayerList = lists:sort(fun(A,B)->compare_player(A,B) end,PlayerList),
                [Seg#rank_seg{playerlist=NewPlayerList,min=lists:nth(length(NewPlayerList),NewPlayerList),max=hd(NewPlayerList)}|Acc];
            false->
                [Seg|Acc]
        end
    end,[],RankInfoList),
    ?INFO("oldRankInfoList:~w ~n RankInfoList:~w ~n",[RankInfoList,NewRankInfoList]),
    set_rank_info_list(Type,NewRankInfoList).

send_war_result_to_rank(Result,P_Player_Info_List)->
    ?INFO("Result:~w P_Player_Info_List:~w~n",[Result,P_Player_Info_List]),
    carlos_rank:send_to_me({update_rank_info,Result,P_Player_Info_List}).

transform_player2p_player_info(Player) when is_record(Player,player)->
    #player{serverID=ServerID,roleID=RoleID,roleName=RoleName,fly=Fly,level=Level,score=Score,head=Head,title=Title,vip=Vip
           ,isMale=IsMale,type=Type,lastcarloswintime=LastCarlosWinTime,lastcarlosequaltime=LastCarlosEqualTime,lastcarloslosetime=LastCarlosLoseTime
           ,lastseasonid=LastSeasonID,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,seasonid=SeasonID,fight_power=FightPower} = Player,
    #p_player_info{serverID=ServerID,roleID=RoleID,roleName=RoleName,fly=Fly,level=Level,score=Score,head=Head,title=Title,vip=Vip
                   ,isMale=IsMale,type=Type,wintime=CarlosWinTime,losetime=CarlosLoseTime,equaltime=CarlosEqualTime,seasonid=SeasonID
                  ,lastwintime=LastCarlosWinTime,lastequaltime=LastCarlosEqualTime,lastlosetime=LastCarlosLoseTime,lastseasonid=LastSeasonID,fight_power=FightPower};
transform_player2p_player_info(Player)->
    ?INFO("将Player转换成p_player_info出现非player结构 Player：~w ~n",[Player]),
    #p_player_info{}.
% -record(p_player_info,{serverID=0,roleID=0,roleName=[],fly=0,level=0,score=0,head=0,title=0,isMale=0,type=0,wintime=0,losetime=0,equaltime=0,rankscore=0,rank=0}).

update_rank_info(Result,P_Player_Info_List)->
    Update_list = [calculate_rankscore(Result,Elem)||Elem<-P_Player_Info_List],
    lists:foreach(fun(One)-> update_player_info(One) end,Update_list).

% -record(rank_seg,{id=0,length=0,minscore=0,maxscore=0,playerlist=[]}).
update_player_info({Exchange,Player})->
    {Type,InsertPlayer} = case Exchange of
        false ->
            {?CURRENT_SEASON_INFO,Player};
        true->
            #p_player_info{lastwintime=LastWinTime,lastequaltime=LastEqualTime,lastlosetime=LastLoseTime,lastseasonid=LastSeasonID,vip=Vip}=Player,
            {?PRE_SEASON_INFO,Player#p_player_info{rankscore=role_carlos:calculate_rankscore(LastWinTime,LastLoseTime,LastEqualTime),equaltime=LastEqualTime,wintime=LastWinTime,losetime=LastLoseTime,seasonid=LastSeasonID,vip=Vip}}
    end,
    RankInfoList = get_rank_info_list(Type),
    case find_player(RankInfoList,InsertPlayer) of
        {persist,_FindOne,Other}->
            set_persist_list(Other);
        {SegID,_FindOne,Other}->
            case lists:keytake(SegID,#rank_seg.id,RankInfoList) of
                {_,FindSeg,OtherSeg}->
                    case length(Other) of
                        0->
                            NewSeg = FindSeg#rank_seg{length=0,min=0,max=0,playerlist=Other};
                        NewLength->
                            Other2 = lists:sort(fun(ElemA,ElemB)->compare_player(ElemA,ElemB) end,Other),
                            NewSeg = FindSeg#rank_seg{length=NewLength,min=lists:nth(NewLength,Other2),max=hd(Other2),playerlist=Other2}
                    end,
                    set_rank_info_list(Type,[NewSeg|OtherSeg]);
                false->
                    ?INFO("在rank_info_list:~w 中没有发现segID：~w ~n",[RankInfoList,SegID])
            end;
        false->
            ignore
    end,
    ?INFO("update_role:~w ~n insert:~w ~n",[Player,InsertPlayer]),
    CurrentSeasonInfo = get_seasoninfo(?CURRENTSEASON),
    update_role_carlos_info(Player,CurrentSeasonInfo#season_info.year,CurrentSeasonInfo#season_info.seasonid),
    insert_player_info(InsertPlayer,get_rank_info_list(Type),Type).

get_rank_info_list()->
    get_rank_info_list(?CURRENT_SEASON_INFO).
get_rank_info_list(Type)->
    SeasonInfo = get_seasoninfo(Type),
    SeasonInfo#season_info.rankinfolist.

set_rank_info_list(RankInfoList)->
    set_rank_info_list(?CURRENT_SEASON_INFO,RankInfoList).
set_rank_info_list(Type,RankInfoList)->
    DicName = case Type of
        ?PRESEASON->
            ?PRE_SEASON_INFO;
        ?CURRENTSEASON->
            ?CURRENT_SEASON_INFO;
        _ ->
            Type
    end,
    SeasonInfo = get_seasoninfo(DicName),
    set_seasoninfo(SeasonInfo#season_info{rankinfolist=RankInfoList},DicName).

get_seasoninfo(Type)->
    DicName = case Type of
        ?PRESEASON->
            ?PRE_SEASON_INFO;
        ?CURRENTSEASON->
            ?CURRENT_SEASON_INFO;
        _ ->
            Type
    end,
    case get(DicName) of
        ?undefined->
            {Year,_Mon,_Day} = erlang:date(),
            #season_info{seasonid=1,year=Year,type=Type,rankinfolist=init_rank_info3()};
        X ->
            X
    end.

set_seasoninfo(SeasonInfo,Type)  when is_record(SeasonInfo,season_info)->
    ?INFO("set_seasoninfo Type:~w ~n",[Type]),
    NewRankInfoList = lists:sort(fun(ElemA,ElemB)->
        ElemA#rank_seg.id<ElemB#rank_seg.id
    end,SeasonInfo#season_info.rankinfolist),
    put(Type,SeasonInfo#season_info{rankinfolist=NewRankInfoList});
set_seasoninfo(_SeasonInfo,_Type)->
    elang:throw(),
    ?INFO("插入的赛季信息为非season_info结构~n").

% -record(rank_seg,{id=0,length=0,minscore=0,maxscore=0,playerlist=[]}).
find_player([],Player)->
    ?INFO("在rank_info_list中未找到对应玩家信息:~w ~n",[Player]),
    false;
find_player([Seg|RankInfoList],Player)->
    case find_player_in_seg(Seg,Player) of
        false->
            find_player(RankInfoList,Player);
        {SegID,FindOne,Other}->
            {SegID,FindOne,Other}
    end.

find_player_in_seg(Seg,Player)->
    case Seg#rank_seg.length >0 of
        true->
            case lists:keytake(Player#p_player_info.roleID,#p_player_info.roleID,Seg#rank_seg.playerlist) of
                {_,FindOne,Other}->
                    {Seg#rank_seg.id,FindOne,Other};
                false->
                    false
            end;
        false->
            false
    end.

get_seg_num(Max_rank_num,Max_show_rank_num)->
    case (Max_rank_num rem Max_show_rank_num) >0 of
        true->
            Max_rank_num div Max_show_rank_num +1;
        false ->
            Max_rank_num div Max_show_rank_num
    end. 

insert_player_info(Player,[],_Type)->
    ?INFO("出现错误，插入数据时递归到空列表,需要插入到persist中~w ~n",[Player]),
    ignore;
    % add_persist_player(Player);
insert_player_info(Player,[Seg|RankInfoList],Type)->
    case compare_player(Player,Seg#rank_seg.min) orelse Seg#rank_seg.length< data_carlos:get(max_show_rank_num) of
        true->
            insert_player_info_into_seg(Player,Seg,Type);
        false->
            insert_player_info(Player,RankInfoList,Type)
    end.

insert_player_info_into_seg(Player1,Seg,Type)->
    Player = Player1#p_player_info{ischange=?PLAYER_CHANGE},
    #rank_seg{playerlist=PlayerList,length=Length} = Seg,
    NewSeg1=Seg#rank_seg{playerlist=lists:sort(fun(ElemA,ElemB)->compare_player(ElemA,ElemB) end,[Player|PlayerList]),length=Length+1},
    NewSeg2=NewSeg1#rank_seg{max=hd(NewSeg1#rank_seg.playerlist),min=lists:nth(NewSeg1#rank_seg.length,NewSeg1#rank_seg.playerlist)},
    RankInfoList = get_rank_info_list(Type),
    case lists:keytake(NewSeg2#rank_seg.id,#rank_seg.id,RankInfoList) of
        false->
            ?ERR("查找Seg ID:~w RankInfoList:~w 失败~n",[NewSeg2#rank_seg.id,RankInfoList]);
        {_,_FindSeg,OtherSeg}->
            NewRankInfoList = [NewSeg2|OtherSeg],
            set_rank_info_list(Type,NewRankInfoList)
    end.

% update_seg_by_playerlist(PlayerList,Seg)->
%     NewSeg1 = Seg#rank_seg{playerlist=lists:sort(fun(ElemA,ElemB)->compare_player(ElemA,ElemB) end,PlayerList),length=length(PlayerList)},
%     NewSeg2=NewSeg1#rank_seg{max=hd(NewSeg1#rank_seg.playerlist),min=lists:nth(NewSeg1#rank_seg.length,NewSeg1#rank_seg.playerlist)},
%     RankInfoList = get_rank_info_list(),
%     case lists:keytake(NewSeg2#rank_seg.id,#rank_seg.id,RankInfoList) of
%         false->
%             ?ERR("查找Seg ID:~w RankInfoList:~w 失败~n",[NewSeg2#rank_seg.id,RankInfoList]);
%         {_,FindSeg,OtherSeg}->
%             NewRankInfoList = [NewSeg2|OtherSeg],
%             set_rank_info_list(NewRankInfoList)
%     end.

calculate_rankscore(Type,Player)->
%%可能会出现卡洛斯战斗中，赛季发生切换的情况
    SeasonInfo = get_seasoninfo(?CURRENT_SEASON_INFO),
    CurrentSeasonID = ?generate_season_id(SeasonInfo#season_info.year,SeasonInfo#season_info.seasonid),
    #p_player_info{equaltime=OldEqualTime,wintime=OldWinTime,losetime=OldLoseTime,seasonid=OldSeasonID,lastwintime=OldLastWinTime,lastequaltime=OldLastEqualTime,lastlosetime=OldLastLoseTime,lastseasonid=OldLastSeasonID} = Player,
    %%?INFO("P.seasonid:~w C.seasonid:~w ~n",[Player#p_player_info.seasonid,CurrentSeasonID]),
    {WinTime,EqualTime,LoseTime,SeasonID,LastWinTime,LastEqualTime,LastLoseTime,LastSeasonID,Exchange} = 
    case Player#p_player_info.seasonid =:= CurrentSeasonID of
        true->
            {OldWinTime,OldEqualTime,OldLoseTime,OldSeasonID,OldLastWinTime,OldLastEqualTime,OldLastLoseTime,OldLastSeasonID,false};
        false->
            {0,0,0,CurrentSeasonID,OldWinTime,OldEqualTime,OldLoseTime,OldSeasonID,true}
    end,
    case Type of
        ?EQUAL->
            case Exchange of
                false->
                    %%将结果更新到本赛季数据中
                    {Exchange,Player#p_player_info{rankscore=data_carlos:get(equal_score)+role_carlos:calculate_rankscore(WinTime,LoseTime,EqualTime),equaltime=EqualTime+1,wintime=WinTime,losetime=LoseTime,seasonid=SeasonID,lastwintime=LastWinTime,lastequaltime=LastEqualTime,lastlosetime=LastLoseTime,lastseasonid=LastSeasonID}};
                true->
                    %%将结果更新到上赛季数据中
                    {Exchange,Player#p_player_info{rankscore=0,equaltime=EqualTime,wintime=WinTime,losetime=LoseTime,seasonid=SeasonID,lastwintime=LastWinTime,lastequaltime=LastEqualTime+1,lastlosetime=LastLoseTime,lastseasonid=LastSeasonID}}
            end;
        Other->
            case Player#p_player_info.type =:= Other of
                true->
                    case Exchange of
                        false->
                            {Exchange,Player#p_player_info{rankscore=data_carlos:get(win_score)+role_carlos:calculate_rankscore(WinTime,LoseTime,EqualTime),wintime=WinTime+1,equaltime=EqualTime,losetime=LoseTime,seasonid=SeasonID,lastwintime=LastWinTime,lastequaltime=LastEqualTime,lastlosetime=LastLoseTime,lastseasonid=LastSeasonID}};
                        true->
                            {Exchange,Player#p_player_info{rankscore=0,wintime=WinTime,equaltime=EqualTime,losetime=LoseTime,seasonid=SeasonID,lastwintime=LastWinTime+1,lastequaltime=LastEqualTime,lastlosetime=LastLoseTime,lastseasonid=LastSeasonID}}
                    end;
                false->
                    case Exchange of
                        false->
                            {Exchange,Player#p_player_info{losetime=LoseTime+1,rankscore=data_carlos:get(lose_score)+role_carlos:calculate_rankscore(WinTime,LoseTime,EqualTime),wintime=WinTime,equaltime=EqualTime,seasonid=SeasonID,lastwintime=LastWinTime,lastequaltime=LastEqualTime,lastlosetime=LastLoseTime,lastseasonid=LastSeasonID}};
                        true->
                            {Exchange,Player#p_player_info{losetime=LoseTime,rankscore=0,wintime=WinTime,equaltime=EqualTime,seasonid=SeasonID,lastwintime=LastWinTime,lastequaltime=LastEqualTime,lastlosetime=LastLoseTime+1,lastseasonid=LastSeasonID}}
                    end                            
            end
    end.

compare_player(PlayerA,PlayerB) when is_record(PlayerA,p_player_info) andalso is_record(PlayerB,p_player_info)->
    Result = (PlayerA#p_player_info.rankscore > PlayerB#p_player_info.rankscore) orelse
    ((PlayerA#p_player_info.rankscore =:= PlayerB#p_player_info.rankscore) andalso (PlayerA#p_player_info.wintime > PlayerB#p_player_info.wintime)) orelse 
    ((PlayerA#p_player_info.rankscore =:= PlayerB#p_player_info.rankscore) andalso (PlayerA#p_player_info.wintime=:=PlayerB#p_player_info.wintime)andalso(PlayerA#p_player_info.equaltime > PlayerB#p_player_info.equaltime)) orelse
    ((PlayerA#p_player_info.rankscore =:= PlayerB#p_player_info.rankscore) andalso (PlayerA#p_player_info.wintime=:=PlayerB#p_player_info.wintime)andalso(PlayerA#p_player_info.equaltime=:=PlayerB#p_player_info.equaltime)andalso(PlayerA#p_player_info.losetime < PlayerB#p_player_info.losetime)) orelse
    ((PlayerA#p_player_info.rankscore =:= PlayerB#p_player_info.rankscore) andalso (PlayerA#p_player_info.wintime=:=PlayerB#p_player_info.wintime)andalso(PlayerA#p_player_info.equaltime=:=PlayerB#p_player_info.equaltime)andalso(PlayerA#p_player_info.losetime=:=PlayerB#p_player_info.losetime)andalso(PlayerA#p_player_info.fly > PlayerB#p_player_info.fly)) orelse
    ((PlayerA#p_player_info.rankscore =:= PlayerB#p_player_info.rankscore) andalso (PlayerA#p_player_info.wintime=:=PlayerB#p_player_info.wintime)andalso(PlayerA#p_player_info.equaltime=:=PlayerB#p_player_info.equaltime)andalso(PlayerA#p_player_info.losetime=:=PlayerB#p_player_info.losetime)andalso(PlayerA#p_player_info.fly=:=PlayerB#p_player_info.fly)andalso(PlayerA#p_player_info.level > PlayerB#p_player_info.level)) orelse
    ((PlayerA#p_player_info.rankscore =:= PlayerB#p_player_info.rankscore) andalso (PlayerA#p_player_info.wintime=:=PlayerB#p_player_info.wintime)andalso(PlayerA#p_player_info.equaltime=:=PlayerB#p_player_info.equaltime)andalso(PlayerA#p_player_info.losetime=:=PlayerB#p_player_info.losetime)andalso(PlayerA#p_player_info.fly=:=PlayerB#p_player_info.fly)andalso(PlayerA#p_player_info.level =:= PlayerB#p_player_info.level)andalso(PlayerA#p_player_info.fight_power > PlayerB#p_player_info.fight_power)),
    Result;
compare_player(PlayerA,PlayerB)->
    ?INFO("出现非player结构PlayerA:~w PlayerB:~w ~n",[PlayerA,PlayerB]),
    true.

get_next_syn_interval()->
    SynInterval = data_carlos:get(syn_rankinfo_interval),
    SynInterval*1000.

get_seasoninfo_by_type(Type,SeasonList)->
    NewType = case Type of
        ?CURRENTSEASON->
            ?CURRENT_SEASON_INFO;
        ?PRESEASON ->
            ?PRE_SEASON_INFO;
        _ ->
            Type
    end,
    case lists:keyfind(NewType,#season_info.type,SeasonList) of
        false->
            ?INFO("查找Type:~w 赛季信息失败,赛季信息列表：~w ~n",[NewType,SeasonList]),
            #season_info{};
        FindOne->
            FindOne
    end.

%% update_playerinfo_data_400(Sesson) ->
%%     case data_setting:get(client_version) of
%%         {4,0,0} ->
%%             RR=[R#rank_seg{playerlist=[list_to_tuple(tuple_to_list(PI)++[1])||PI<-PL]}
%%                 ||#rank_seg{playerlist=PL}=R<-Sesson#season_info.rankinfolist],
%%             Sesson#season_info{rankinfolist=RR};
%%         {3,3,0} ->
%%             RR=[R#rank_seg{playerlist=[list_to_tuple(tuple_to_list(PI)++[1])||PI<-PL]}
%%                 ||#rank_seg{playerlist=PL}=R<-Sesson#season_info.rankinfolist],
%%             Sesson#season_info{rankinfolist=RR};
%%         _ -> Sesson
%%     end.

get_next_season_begin({Year,Mon,_Day})->
    {NewYear,NewMon} = case Mon +1 > 12 of
        true->
            {Year+1,1};
        false->
            {Year,Mon+1}
    end,
    {NewYear,NewMon,1}.


begin_new_season()->
    ?INFO("开始新赛季~n"),
    _PreSeasonInfo = get_seasoninfo(?PRE_SEASON_INFO),
    CurrentSeasonInfo = get_seasoninfo(?CURRENT_SEASON_INFO),
    % send_season_reward(CurrentSeasonInfo),
    {Year,_,_} = erlang:date(),
    NewCurrentSeasonInfo = #season_info{seasonid=get_next_season_id(CurrentSeasonInfo#season_info.seasonid),year=Year,type=?CURRENT_SEASON_INFO,rankinfolist=init_rank_info3()},
    set_seasoninfo(NewCurrentSeasonInfo,?CURRENT_SEASON_INFO),
    set_seasoninfo(CurrentSeasonInfo#season_info{type=?PRE_SEASON_INFO},?PRE_SEASON_INFO),
    do_syn_rankinfo(?PRESEASON,true),
    do_syn_rankinfo(?CURRENTSEASON,true),
    clear_carlos_buy_time(),
    init_season_end(),
    %%延长赛季奖励发送为一个战场时间，防止跨赛季比赛的异常问题
    case data_carlos:get(season_delay_test) of
        true->
            Interval = data_carlos:get(carlos_reward_delay);
        false->
            Interval = ?CARLOS_REWARD_DELAY
    end,
    erlang:send_after(Interval*1000,?MODULE,{send_season_reward,?PRESEASON}).

get_next_season_id(PreID)->
    {_Year,Mon,_Day} = erlang:date(),
    case Mon =:= 1 of
        true->
            1;
        false->
            case PreID +1 > 12 of 
                true->
                    ?ERR("赛季编号异常:SeasonID：~w 当前月：~w ~n",[PreID,Mon]),
                    Mon;
                false->
                    PreID+1
            end
    end.

send_season_reward(SeasonInfo)->
    ?INFO("开始发奖：赛季信息：~w ~n",[SeasonInfo]),
    AllPlayerList = lists:foldl(fun(Seg,Acc)->
        Seg#rank_seg.playerlist++Acc
    end,[],update_rank(SeasonInfo#season_info.rankinfolist)),
    case AllPlayerList of
        []->
            ?INFO("赛季：~w年 ~w 赛季发奖列表为空 赛季信息：~w ~n ",[SeasonInfo#season_info.year,SeasonInfo#season_info.seasonid,SeasonInfo]);
        _ ->
            ArgList = [SeasonInfo#season_info.seasonid],
            spawn(fun()->lists:foreach(fun(Player)->
                send_player_season_reward(Player,ArgList)
            end,AllPlayerList) end)
    end.
% -record(p_player_info,{serverID=0,roleID=0,roleName=[],fly=0,level=0,score=0,head=0,title=0,isMale=0,type=0,wintime=0,losetime=0,equaltime=0,rankscore=0,rank=0}).
update_role_carlos_info(Player,SeasonYear,SeasonID) when is_record(Player,p_player_info)->
    #p_player_info{serverID=ServerID,roleID=RoleID,wintime=WinTime,losetime=LoseTime,equaltime=EqualTime,rankscore=RankScore,seasonid=CurrentSeasonID,lastwintime=LastCarlosWinTime,lastequaltime=LastCarlosEqualTime,lastlosetime=LastCarlosLoseTime,lastseasonid=LastSeasonID} =Player,
    RoleSeasonID = ?generate_season_id(SeasonYear,SeasonID),
    RealLastSeasonID = case RoleSeasonID =/= Player#p_player_info.seasonid of
        true->
            %%在战斗过程中赛季已经切换
            CurrentSeasonID;
        false->
            LastSeasonID
    end,
    send_msg:direct(ServerID, carlos_server, {update_role_carlos_info,RoleID,{WinTime,EqualTime,LoseTime,RoleSeasonID,RankScore,LastCarlosWinTime,LastCarlosEqualTime,LastCarlosLoseTime,RealLastSeasonID}}).

send_player_season_reward(Player,ArgList) when is_record(Player,p_player_info)->
    ?INFO("Player：~w ~n",[Player]),
    R = get_season_reward(Player#p_player_info.rank),
    case R of
        ?undefined->
            ?INFO("向玩家发送赛季奖励,出现查找不到奖励配置情况");
        _ ->
            #sell_reward{item=Item} = R,
            NewItem = hd(Item),
            Num = NewItem#new_item.itemNum,
            ArgList2 = ArgList++[Player#p_player_info.rank,Num],
            send_msg:direct(Player#p_player_info.serverID, carlos_server,{season_reward,Player#p_player_info.roleID,?MAIL_CARLOS_SEASON_RANK_REWARD, R,ArgList2})
    end;
send_player_season_reward(Player,_ArgList)->
    ?INFO("向玩家发送奖励出现非p_player_info结构Player:~w ~n",[Player]),
    ignore.

erase(Name)->
    case get(Name) of
        ?undefined->
            ignore;
        _ ->
            put(Name,?undefined)
    end.

get_season_reward(Rank)->
    RewardList = data_carlos:get(rank_reward),
    FindRewardList = [R||{{Begin,End},R}<-RewardList,Rank >= Begin,Rank =< End],
    case FindRewardList of
        []->
            ?undefined;
        _ ->
            hd(FindRewardList)
    end.

get_persist_list()->
    case get(?PersistList) of
        ?undefined->
            [];
        X ->
            X
    end.

set_persist_list(NewPersistList)->
    put(?PersistList,NewPersistList).

add_persist_player(Player)->
    List = get_persist_list(),
    set_persist_list([Player|List]).

add_persist_list(AddList)->
    List = get_persist_list(),
    set_persist_list(List++AddList).

get_niubi(N) ->
    Sql = io_lib:format("select roleID from gRole order by fightPower desc limit ~w;", [N]),
    case db_sql:get_all(Sql) of
        [_|_]=List ->
            [E||[E]<-List];
        _ ->
            []
    end.

test_get_result()->
    Rand = random:uniform(),
    if 
        Rand < 0.34 ->
            attacker;
        Rand < 0.67 ->
            equal;
        true->
            defender
    end.


test(N)->
    RoleIDList = get_niubi(N),
    RoleInfoList = [db_sql:get_roleInfo(RoleID)||RoleID<-RoleIDList],
    PlayerInfoList = lists:foldl(fun(RoleInfo,Acc)->
        #role{roleID=RoleID,roleName=RoleName,plane_level=Fly,level=Level,head=Head,title=Title,isMale=IsMale,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeasonID,carloslastwintime=CarlosLastWinTime,carloslastequaltime=CarlosLastEqualTime,carloslastlosetime=CarlosLastLoseTime,carloslastseasonid=CarlosLastSeasonID} = RoleInfo,
        [#p_player_info{serverID=util:calc_server_id(roleID, RoleID),roleID=RoleID,roleName=RoleName,fly=Fly,level=Level,score=trunc(random:uniform()*1000),head=Head,title=Title,isMale=IsMale,type=defender,wintime=CarlosWinTime,losetime=CarlosLoseTime,equaltime=CarlosEqualTime,seasonid=CarlosSeasonID,lastwintime=CarlosLastWinTime,lastequaltime=CarlosLastEqualTime,lastlosetime=CarlosLastLoseTime,lastseasonid=CarlosLastSeasonID}|Acc]
    end,[],RoleInfoList),
    % [send_war_result_to_rank(test_get_result(),[P])||P<-PlayerInfoList].
    lists:foreach(fun(E)->
        send_war_result_to_rank(test_get_result(),[E]),
        timer:sleep(100)
    end,PlayerInfoList).
    % #p_player_info{serverID=ServerID,roleID=RoleID,roleName=RoleName,fly=Fly,level=Level,score=Score,head=Head,title=Title,isMale=IsMale,type=Type};
send_to_me(Msg) ->
    send_msg:direct_by_name(carlos_match, carlos_rank, Msg).

test_erase(Name)->
    erlang:send(?MODULE,{erase,Name}).

clear_carlos_buy_time()->
    case data_carlos:get(is_clear_carlos_buy_time) of
        true->
            send_msg:broadcast(carlos_server,clear_carlos_buy_time);
        false->
            ignore
    end.

deduct_carlostime({Time,RoleID,Type,SeasonType}=_Msg)->
    %%?INFO("Msg:~w~n",[Msg]),
    RankInfoList = get_rank_info_list(SeasonType),
    SeasonInfo = get_seasoninfo(SeasonType),
    Player = #p_player_info{roleID=RoleID},
    Result=case find_player(RankInfoList,Player) of
        {SegID,FindOne,Other}->
            case lists:keytake(SegID,#rank_seg.id,RankInfoList) of
                {_,FindSeg,OtherSeg}->
                    case length(Other) of
                        0->
                            NewSeg = FindSeg#rank_seg{length=0,min=0,max=0,playerlist=Other};
                        NewLength->
                            Other2 = lists:sort(fun(ElemA,ElemB)->compare_player(ElemA,ElemB) end,Other),
                            NewSeg = FindSeg#rank_seg{length=NewLength,min=lists:nth(NewLength,Other2),max=hd(Other2),playerlist=Other2}
                    end,
                    set_rank_info_list(SeasonType,[NewSeg|OtherSeg]);
                false->
                    ?INFO("在rank_info_list:~w 中没有发现segID：~w ~n",[RankInfoList,SegID])
            end,
            {true,FindOne};
        false->
            {false,Player}
    end,
    case Result of
        {true,Player1}->
            NewPlayer = real_dedecut_carlostime(Player1,Time,Type),
            ?INFO("NewPlayer:~w ~n",[NewPlayer]),
            update_role_carlos_info(NewPlayer,SeasonInfo#season_info.year,SeasonInfo#season_info.seasonid),
            insert_player_info(NewPlayer,get_rank_info_list(SeasonType),SeasonType);
        {false,_}->
            update_role_carlos_info_withRoleID(RoleID,Time,Type,SeasonType)
    end;
deduct_carlostime(Msg)->
    ?INFO("undefined Msg:~w ~n",[Msg]).

real_dedecut_carlostime(Player,Time,Type)->
    Player1 = case Type of
        win ->
            OldWinTime = Player#p_player_info.wintime,
            Player#p_player_info{wintime=erlang:max(0,OldWinTime-Time)};
        equal->
            OldEqualTime = Player#p_player_info.equaltime,
            Player#p_player_info{equaltime=erlang:max(0,OldEqualTime-Time)};
        lose->
            OldLoseTime = Player#p_player_info.losetime,
            Player#p_player_info{losetime=erlang:max(0,OldLoseTime-Time)};
        _ ->
            ?INFO("undefined type: ~w ~n",[Type]),
            Player
    end,
    #p_player_info{wintime=WinTime,equaltime=EqualTime,losetime=LoseTime}=Player1,
    Player1#p_player_info{rankscore=role_carlos:calculate_rankscore(WinTime,LoseTime,EqualTime)}.

update_role_carlos_info_withRoleID(RoleID,Time,Type,SeasonType)->
     send_msg:direct(util:calc_server_id(roleID,RoleID), carlos_server, {deduct_role_carlostime,RoleID,{Time,Type,SeasonType}}).

%%一定要确保玩家的赛季信息有更新
test_deduct_carlostime(Time,RoleID)->
    test_deduct_carlostime(Time,RoleID,win,?CURRENTSEASON).
test_deduct_carlostime(Time,RoleID,equal,SeasonType)->
    test_deduct_carlostime1(Time,RoleID,equal,SeasonType);    
test_deduct_carlostime(Time,RoleID,win,SeasonType)->
    test_deduct_carlostime1(Time,RoleID,win,SeasonType);
test_deduct_carlostime(Time,RoleID,lose,SeasonType)->
    test_deduct_carlostime1(Time,RoleID,lose,SeasonType);
test_deduct_carlostime(_Time,_RoleID,Type,_SeasonType)->
    ?INFO("undefined Type:~w ~n",[Type]).

test_deduct_carlostime1(Time,RoleID,Type,SeasonType)->
    case lists:member(SeasonType,[?CURRENTSEASON,?PRESEASON]) of
        true->
            Msg = {Time,RoleID,Type,SeasonType},
            erlang:send(?MODULE,{deduct_carlostime,Msg});
        false->
            ?INFO("undefined season type:~w ~n",[SeasonType])
    end.

%%从游戏服方向修改玩家信息，只能修改当前赛季数据
test_deduct_carlostime_from_gameserver(Time,RoleID,Type)->
    test_deduct_carlostime_from_gameserver(Time,RoleID,Type,false).
test_deduct_carlostime_from_gameserver(Time,RoleID,Type,Force)->
    ServerID = util:calc_server_id(roleID,RoleID),
    case lists:member(ServerID,[data_setting:get(server_id) | data_setting:get(merge_server_id_list)]) orelse Force of
        true-> 
            case ets:lookup(?ETS_CARLOS_ROLE_WAR,{match_info,RoleID}) of
                []->
                    Msg = {Time,Type,1},
                    %%向游戏服卡洛斯进程发送更新玩家卡洛斯数据信息
                    erlang:send(carlos_server,{deduct_role_carlostime,RoleID,Msg}),
                    RoleInfo1 = 
                        case catch role_lib:call_server(RoleID, get_role_info,1000) of
                            {'EXIT',_} ->
                                db_sql:get_roleInfo(RoleID);
                            X->
                                X
                        end,
                    RoleInfo = case Type of
                        win->
                            RoleInfo1#role{carloswintime=erlang:max(0,RoleInfo1#role.carloswintime-Time)};
                        equal->
                            RoleInfo1#role{carlosequaltime=erlang:max(0,RoleInfo1#role.carlosequaltime-Time)};
                        lose->
                            RoleInfo1#role{carloslosetime=erlang:max(0,RoleInfo1#role.carloslosetime-Time)};
                        _ ->
                            ?INFO("undefined Type:~w ~n",[Type]),
                            RoleInfo1
                    end,
                    #role{roleName=RoleName,plane_level=Fly,level=Level,head=Head,title=Title,isMale=IsMale,carloswintime=CarlosWinTime,carlosequaltime=CarlosEqualTime,carloslosetime=CarlosLoseTime,carlosseasonid=CarlosSeasonID,carloslastwintime=CarlosLastWinTime,carloslastequaltime=CarlosLastEqualTime,carloslastlosetime=CarlosLastLoseTime,carloslastseasonid=CarlosLastSeasonID,fightPower=FightPower} = RoleInfo,
                    RoleCarlosInfo = #p_player_info{serverID=util:calc_server_id(roleID, RoleID),roleID=RoleID,roleName=RoleName,fly=Fly,level=Level,score=trunc(random:uniform()*1000),head=Head,title=Title,isMale=IsMale,type=defender,wintime=CarlosWinTime,losetime=CarlosLoseTime,equaltime=CarlosEqualTime,seasonid=CarlosSeasonID,lastwintime=CarlosLastWinTime,lastequaltime=CarlosLastEqualTime,lastlosetime=CarlosLastLoseTime,lastseasonid=CarlosLastSeasonID,rankscore=role_carlos:calculate_rankscore(CarlosWinTime,CarlosLoseTime,CarlosEqualTime),fight_power=FightPower},
                    send_to_update_carlosinfo(RoleCarlosInfo);
                _ ->
                    ?ERR("修改玩家RoleID：~w 卡洛斯数据由于玩家在卡洛斯比赛中而失败，需要等待卡洛斯结束后重新调用修改~n",[RoleID])
            end;
        false->
            ?ERR("玩家RoleID：~w不属于本服务器~n",[RoleID]),
            error
    end.

send_to_update_carlosinfo(Player) when is_record(Player,p_player_info)->
    carlos_rank:send_to_me({fix_role_carlosinfo,Player});
send_to_update_carlosinfo(Player)->
    ?INFO("illegal p_player_info :~w ~n",[Player]),
    ignore.

test_send_season_reward(Type)->
    erlang:send(?MODULE,{send_season_reward,Type}).

test_fix_p_player()->
    erlang:send(?MODULE,fix_p_player).

do_fix_p_player()->
    fix_player(?PRESEASON),
    fix_player(?CURRENTSEASON).

fix_player(Type)->
    RankInfoList = get_rank_info_list(Type),
    NewRankInfoList = lists:foldr(fun(Seg,Acc)->
        PlayerList = Seg#rank_seg.playerlist,
        Max = Seg#rank_seg.max,
        Min = Seg#rank_seg.min,
        NewPlayerList = [tranform_oldplayer2newplayer(OldPlayer)||OldPlayer<-PlayerList],
        NewMax = tranform_oldplayer2newplayer(Max),
        NewMin = tranform_oldplayer2newplayer(Min),
        [Seg#rank_seg{max=NewMax,min=NewMin,playerlist=NewPlayerList}|Acc]
    end,[],RankInfoList),
    set_rank_info_list(Type,NewRankInfoList).

fix_seasoninfo(Season) when is_record(Season,season_info)->
    RankInfoList = Season#season_info.rankinfolist,
    NewRankInfoList = lists:foldr(fun(Seg,Acc)->
        PlayerList = Seg#rank_seg.playerlist,
        Max = Seg#rank_seg.max,
        Min = Seg#rank_seg.min,
        NewPlayerList = [tranform_oldplayer2newplayer(OldPlayer)||OldPlayer<-PlayerList],
        NewMax = tranform_oldplayer2newplayer(Max),
        NewMin = tranform_oldplayer2newplayer(Min),
        [Seg#rank_seg{max=NewMax,min=NewMin,playerlist=NewPlayerList}|Acc]
    end,[],RankInfoList),
    Season#season_info{rankinfolist=NewRankInfoList}.


%%添加了是否更新以及战斗力
tranform_oldplayer2newplayer(Player) when is_record(Player,p_player_info)->
    Player;
tranform_oldplayer2newplayer(0)->
    0;
tranform_oldplayer2newplayer(Player)->
    List = tuple_to_list(Player),
    NormalLen = tuple_size(#p_player_info{}),
    NewList = if
        tuple_size(Player)=:=23 ->
            List++[1];
        tuple_size(Player)=:=22->
            List++[1,1];
        tuple_size(Player)>NormalLen->
            lists:sublist(List,NormalLen);
        true->
            ?ERR("undefined length:~w  ~n",[tuple_size(Player)]),
            List
    end,
    list_to_tuple(NewList).

test_set_seasoninfo(SeasonID,Type)->
    erlang:send(?MODULE,{fix_seasoninfo,SeasonID,Type}).

test_start_persist()->
    erlang:send(?MODULE,dump_data),
    erlang:send(?MODULE,do_hibernate).

%%该命令需要在游戏服上面执行,不需要其他服务器的配合
test_set_all_role_carlos_time_from_gameserver(WinTime,EqualTime,LoseTime,SeasonID)->
    %%清空所有次数信息
    ets:delete_all_objects(?ETS_CARLOS_ROLETIMES),
    %%清空所有在线玩家数据
    RoleIDList = [E||{E,_}<-ets:tab2list(?ETS_ROLE_ONLINE)],
    spawn(fun()->
            lists:foreach(fun(RoleID)->
                RoleInfo = 
                        case catch role_lib:call_server(RoleID, get_role_info,1000) of
                            {'EXIT',_} ->
                                db_sql:get_roleInfo(RoleID);
                            X->
                                X
                        end,
                #role{carloslastwintime=CarlosLastWinTime,carloslastequaltime=CarlosLastEqualTime,carloslastlosetime=CarlosLastLoseTime,carloslastseasonid=CarlosLastSeasonID} = RoleInfo,
                Msg = {update_role_carlos_info,RoleID,{WinTime,EqualTime,LoseTime,SeasonID,0,CarlosLastWinTime,CarlosLastEqualTime,CarlosLastLoseTime,CarlosLastSeasonID}},
                erlang:send(carlos_server,Msg)
                end,
            RoleIDList)
        end),
    %%清空所有离线玩家数据库
    Sql = io_lib:format("update gRole set carloswintime=~w,carlosequaltime=~w,carloslosetime=~w,carlosseason=~w",[WinTime,EqualTime,LoseTime,SeasonID]),
    case db_sql:sql_execute_with_log(Sql) of
        {ok,_}->
            ok;
        _ ->
            failed
    end.


