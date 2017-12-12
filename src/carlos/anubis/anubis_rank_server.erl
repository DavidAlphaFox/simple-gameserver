%%=======================================================================
%%注意：
%%（1）游戏服只保留用于显示的前1000排名的公会数据，排行榜服务器才保留了完整的数据
%%（2）发奖的时候需要排行榜服务器告知游戏服对本服对应的公会发奖
%%=======================================================================
-module(anubis_rank_server).
-compile(export_all).
-include("def_carlos.hrl").
-include("def_mail.hrl").
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_reward.hrl").

-define(DUMP_INTERVAL,300).
-define(SEASONINFO,seasoninfo).
-define(FAMILY_CHANGE,true).
-define(FAMILY_NOCHANGE,false).
-define(MAX_FAMILY,1).
-define(MIN_FAMILY,0).

-define(ANUBIS_FIGHT_WIN,win).
-define(ANUBIS_FIGHT_EQUAL,equal).
-define(ANUBIS_FIGHT_LOSE,lose).
-define(HAS_SEND_REWARD_FLAGE,0).
-define(PRE_SEND_REWARD_FLAGE,1).
-define(PRE_DELETE_SEASON_FLAGE,1).
-define(REMAIN_SEASON_FLAGE,0).

-record(state,{current_season_id=0,clear_time_ref=?undefined,transform_season_ref=?undefined}).

start( ) ->
    {ok,_} = 
    supervisor:start_child(anubis_sup,
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
        family_cross_master ->
            process_flag(trap_exit,true),
            State = init_anubis_rank_info(),
            erlang:send_after(?DUMP_INTERVAL*1000,self(),dump_data),
            erlang:send_after(?DUMP_INTERVAL*1000,self(),do_hibernate),

            %%发送消息告知所有游戏服，检查自己状态，主要用于排行榜服务器挂掉之后重启
            erlang:send_after(5*1000,self(),broadcast_check_state),
            {ok, State};
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
handle_call({get_familylist,SeasonID},_From,State)->
    Reply = get_familylist(SeasonID),
    {reply,Reply,State};
handle_call(i,_From,State)->
    {reply,State,State};
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

%%由于按年清楚赛季数据会出现send_after的参数过大，不能调用，所以需在此处判断是否需要开启定时器
handle_info(dump_data,State=#state{clear_time_ref=ClearTimeRef}) ->
    DeleteSeasonIDList = get_all_exist_seasonIDList(),
    erlang:send_after(?DUMP_INTERVAL*1000, self(),dump_data),
    NewState = case ClearTimeRef of
        ?undefined->
            case is_need_start_clear_timer() of
                {true,Interval}->
                    DelTimeRef = erlang:send_after(Interval*1000,?MODULE,{delete_all_season,DeleteSeasonIDList}),
                    State#state{clear_time_ref=DelTimeRef};
                false->
                    State
            end;
        _ ->
            State
    end,
    delete_invalid_seasoninfo(),
    do_persist(NewState),
    {noreply,NewState};

handle_info({Ref,_Res},State) when is_reference(Ref)->
    {noreply,State};

handle_info(Info, State) ->
    case catch do_handle_info(Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exception:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

terminate(_Reason, State) ->
    do_persist(State),
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

do_handle_info(new_season,State)->
    % ?ERR("season transform state:~w ~n",[State]),
    NewState = do_season_transform(State),
    {noreply,NewState};

do_handle_info({delete_all_season,DeleteSeasonIDList},State)->
    NewState = do_delete_all_season(State,DeleteSeasonIDList),
    {noreply,NewState};

do_handle_info(delete_all_season,State)->
    NewState = do_delete_all_season2(State),
    {noreply,NewState};

do_handle_info({send_season_reward,CurrentSeasonID},State)->
    do_send_season_reward(CurrentSeasonID),
    {noreply,State};

do_handle_info({syn_rankinfo,SeasonID,Force},#state{current_season_id=CSeasonID}=State)->
    do_syn_rankinfo(SeasonID,Force,CSeasonID),
    SynInterval = data_anubis:get(syn_rankinfo_interval),
    erlang:send_after(SynInterval*1000,self(),{syn_rankinfo,CSeasonID,false}),
    {noreply,State};

do_handle_info({syn_rankinfo,SeasonID,ServerID,Force},#state{current_season_id=CSeasonID}=State)->
    do_syn_rankinfo(SeasonID,ServerID,Force,CSeasonID),
    {noreply,State};

do_handle_info({anubis_fight_result,FamilyPlayer,FightMsg,SeasonID},#state{current_season_id=CSeasonID}=State)->
    do_anubis_fight_result(FamilyPlayer,FightMsg,SeasonID,CSeasonID),
    {noreply,State};

do_handle_info({test_balance_season,SeasonID},State)->
    do_test_balance_season(SeasonID),
    {noreply,State};

do_handle_info({get_exist_season_id,ServerID},#state{current_season_id=CSeasonID}=State)->
    ExistSeasonIDList = get_all_exist_seasonIDList(),
    SeasonIDList = lists:foldl(fun(SeasonID,Acc)->
        case get_anubis_seasoninfo(SeasonID) of
            ?undefined->
                Acc;
            #anubis_season_info{delete_flage=DeleteFlage,send_reward_flage=SendRewardFlage}->
                case DeleteFlage=:=?REMAIN_SEASON_FLAGE andalso SendRewardFlage=:=?HAS_SEND_REWARD_FLAGE of
                    true->
                        [SeasonID|Acc];
                    false->
                        Acc
                end
        end
    end,[],ExistSeasonIDList),
    SeasonIDReturnMsg = {get_exist_season_id_return,SeasonIDList,CSeasonID},
    send_msg:direct(ServerID,anubis_server,SeasonIDReturnMsg),
    {noreply,State};

do_handle_info({test_syn_rankinfo,SeasonID,Force},#state{current_season_id=CSeasonID}=State)->
    do_syn_rankinfo(SeasonID,Force,CSeasonID),
    {noreply,State};

%%handle test msg
do_handle_info({Func,ArgList},State)->
    case catch ?MODULE:Func(ArgList) of
        {test,NewState}->
            {noreply,NewState};
        true->
            {noreply,State};
        Exception->
            Exception
    end;

do_handle_info(broadcast_check_state,State)->
    % ?ERR("broadcast_check_state~n"),
    Msg = check_state,
    send_msg:broadcast(anubis_server,Msg),
    {noreply,State};

do_handle_info(Info, State) ->
    ?ERR("未知的消息:~p.~n", [Info]),
    {noreply, State}.

init_anubis_rank_info()->
    {State,AllSeasonInfo} = case db_sql:get_etc(?DB_ETC_KEY_ANUBIS_RANK_DATE) of
        {DState,DSeasonInfoList} when is_list(DSeasonInfoList) andalso is_record(DState,state) ->
            {DState,DSeasonInfoList};
        _->
            InitSeasonInfo = init_anubis_rank_info2(),
            InitState = init_anubis_state(),
            {InitState,[InitSeasonInfo]}
    end,
    % ?ERR("AllSeasonInfo:~w ~n",[AllSeasonInfo]),
    [set_anubis_seasoninfo(SeasonInfo)||SeasonInfo<-AllSeasonInfo],
    init_syn_anubis_rankinfo(State#state.current_season_id),
    %%设置了赛季切换以及赛季数据清除的定时器
    init_anubis_season_end(State).

init_anubis_state()->
    CurrentSeasonID = generate_seasonid(),
    #state{current_season_id=CurrentSeasonID}.

init_anubis_season_end(#state{transform_season_ref=TRef} = State)->
    case data_anubis:get(season_test) of
        false->
            {CYear,CMon,CDay} = erlang:date(),
            {Year,Mon,Day} = get_next_season_begin({CYear,CMon,CDay}),
            TimeStamp = util:datetime_to_seconds({{Year,Mon,Day},{0,0,0}}),
            Interval = TimeStamp - util:now(),
            ?INFO("非测试模式，下赛季距离现在：~w (S)~n",[Interval]);
        true->
            Interval = data_anubis:get(season_interval)
    end,
    Ref = erlang:send_after(Interval*1000,?MODULE,new_season),
    case TRef of
        ?undefined->
            ignore;
        _->
            catch (erlang:cancel_timer(TRef))
    end,
    init_clear_timer(State#state{transform_season_ref=Ref}).

%%由于send_after定时器的时间间隔有范围值,故在每年的最后一天开始启动定时器，并且需要在定时持久化的函数中检测是否有对应的定时器,如果没有就需要增加上
init_clear_timer(State=#state{clear_time_ref=ClearTimeRef})->
    %%首先取消旧的定时器,防止多个定时器的存在
    % DeleteSeasonIDList = get_all_exist_seasonIDList(),
    catch (erlang:cancel_timer(ClearTimeRef)),
    case is_need_start_clear_timer() of
        {true,Interval}->
            % DelTimeRef = erlang:send_after(Interval*1000,?MODULE,{delete_all_season,DeleteSeasonIDList}),
            DelTimeRef = erlang:send_after(Interval*1000,?MODULE,delete_all_season),
            State#state{clear_time_ref=DelTimeRef};
        false->
            State
    end.

%%每年的最后一天启动开始启动清楚定时器
is_need_start_clear_timer()->
    case data_anubis:get(season_test) of
        false->
            {CYear,_CMon,_CDay} = erlang:date(),
            NowSec = util:now(),
            case NowSec >= util:datetime_to_seconds({{CYear,12,30},{0,0,1}}) of
                true->
                    {true,util:datetime_to_seconds({{CYear+1,1,1},{0,0,1}})-NowSec};
                false->
                    false
            end;
        true->
            {true,data_anubis:get(delete_seasoninfo_interval)}
    end.

init_anubis_rank_info2()->
    #anubis_season_info{seasonid=generate_seasonid(),seasonrank=init_anubis_rank_info3()}.
init_anubis_rank_info2(SeasonID)->
    #anubis_season_info{seasonid=SeasonID,seasonrank=init_anubis_rank_info3(),send_reward_flage=?HAS_SEND_REWARD_FLAGE,delete_flage=?REMAIN_SEASON_FLAGE}.

init_anubis_rank_info3()->
    Max_Store_rank_num = data_anubis:get(max_store_rank_num),
    Max_Seg_Length = data_anubis:get(max_seg_length),
    SegNum = get_seg_num(Max_Store_rank_num,Max_Seg_Length),
    [#anubis_rank_seg{segid=ID,segmin=?MIN_FAMILY,segmax=?MAX_FAMILY}||ID<-lists:seq(1,SegNum)].

%%只会自动同步当前赛季的数据,并且使用增量更新
init_syn_anubis_rankinfo(SeasonID)->
    erlang:send(?MODULE,{syn_rankinfo,SeasonID,false}).

set_anubis_seasoninfo(#anubis_season_info{seasonid=SeasonID}=SeasonInfo)->
    erlang:put({?SEASONINFO,SeasonID},SeasonInfo).

get_anubis_seasoninfo(SeasonID)->
    erlang:get({?SEASONINFO,SeasonID}).

%%如果family不在赛季信息中，则添加，如果在，则删除添加,返回的Index表示该Family的位置
update_family_info(Family,SeasonID) when is_integer(SeasonID)->
    case get_anubis_seasoninfo(SeasonID) of
        ?undefined->
            % ?ERR("not exist seasonID:~w ~n",[SeasonID]),
            false;
        SeasonInfo=#anubis_season_info{rankfamilynum=FamilyNum}->
            TempSeasonInfo = case find_family(Family,SeasonInfo) of
                false->
                    SeasonInfo;
                {_SegID,_FindOne,_Other,OtherSeg,SegWithoutFamily}->
                    SeasonInfo#anubis_season_info{seasonrank=[SegWithoutFamily|OtherSeg],rankfamilynum=FamilyNum-1}
            end,
            case insert_family_info(Family,TempSeasonInfo) of
                false->
                    false;
                NewSeasonInfo->
                    set_anubis_seasoninfo(NewSeasonInfo),
                    true
            end
    end.

insert_family_info(Family,SeasonID) when is_integer(SeasonID)->
    case get_anubis_seasoninfo(SeasonID) of
        ?undefined->
            % ?ERR("not exist seasonID:~w ~n",[SeasonID]),
            false;
        SeasonInfo->
            case insert_family_info(Family,SeasonInfo) of
                false->
                    % ?ERR("insert Family:~w into seasonID:~w  failed~n",[Family,SeasonID]),
                    false;
                NewSeasonInfo->
                    set_anubis_seasoninfo(NewSeasonInfo),
                    true
            end
    end;
insert_family_info(Family,#anubis_season_info{seasonrank=SegList,seasonid=SeasonID,rankfamilynum=Num}=SeasonInfo)->
    case insert_family_info(Family,sort_seg(SegList),SeasonID,[],0) of
        {true,NewSegList}->
            SeasonInfo#anubis_season_info{rankfamilynum=Num+1,seasonrank=NewSegList};
        false->
            false
    end.

insert_family_info(_Family,[],_SeasonID,_RemainSegList,_Index)->
    % ?ERR("出现错误，插入数据时递归到空列表~w ~n",[Family]),
    false;
insert_family_info(Family,[Seg|RemainSegList],SeasonID,HasCheckSegList,IndexAcc)->
    case compare_family_player(Family,Seg#anubis_rank_seg.segmin) orelse Seg#anubis_rank_seg.segnum< data_anubis:get(max_seg_length) of
        true->
            NewSeg= insert_player_info_into_seg(Family,Seg,SeasonID),
            NewSegList = [NewSeg|RemainSegList]++HasCheckSegList,
            {true,sort_seg(NewSegList)};
        false->
            insert_family_info(Family,RemainSegList,SeasonID,[Seg|HasCheckSegList],IndexAcc+Seg#anubis_rank_seg.segnum)
    end.

insert_player_info_into_seg(Family,Seg,_SeasonID)->
    #anubis_rank_seg{seglist=FamilyList,segnum=Length} = Seg,
    % {Index,NewFamilyList} = insert_into_sort_list(FamilyList,Family,fun anubis_rank_server:compare_family_player/2),
    NewSeg1=Seg#anubis_rank_seg{seglist=lists:sort(fun(ElemA,ElemB)->compare_family_player(ElemA,ElemB) end,[Family|FamilyList]),segnum=Length+1},
    % NewSeg1=Seg#anubis_rank_seg{seglist=NewFamilyList,segnum=Length+1},
    NewSeg1#anubis_rank_seg{segmax=hd(NewSeg1#anubis_rank_seg.seglist),segmin=lists:nth(NewSeg1#anubis_rank_seg.segnum,NewSeg1#anubis_rank_seg.seglist)}.

sort_seg(SegList)->
    lists:sort(fun(A,B)->A#anubis_rank_seg.segid<B#anubis_rank_seg.segid end,SegList).

insert_into_sort_list(List,Elem,Cmp)->
    insert_into_sort_list(List,Elem,Cmp,0,[]).
insert_into_sort_list([],Elem,_Cmp,Index,Head)->
    {Index+1,lists:reverse([Elem|Head])};
insert_into_sort_list([H|T]=List,Elem,Cmp,Index,Head)->
    case Cmp(H,Elem) of
        true->
            insert_into_sort_list(T,Elem,Cmp,Index+1,[H|Head]);
        false->
            {Index+1,lists:reverse([Elem|Head])++List}
    end.
%%判断是否A>B
compare_family_player(_FamilyA,?MAX_FAMILY)->
    false;
compare_family_player(_FamilyA,?MIN_FAMILY)->
    true;
compare_family_player(FamilyA,FamilyB)->
    #family_player{familyscore=FamilyScoreA,familylevel=FamilyLevelA,familyfightpower=FamilyFightPowerA}=FamilyA,
    #family_player{familyscore=FamilyScoreB,familylevel=FamilyLevelB,familyfightpower=FamilyFightPowerB}=FamilyB,
    (FamilyScoreA>FamilyScoreB) orelse  
    (FamilyScoreA=:=FamilyScoreB andalso FamilyLevelA>FamilyLevelB) orelse
    (FamilyScoreA=:=FamilyScoreB andalso FamilyLevelA=:=FamilyLevelB andalso FamilyFightPowerA>FamilyFightPowerB).

find_family(Family,SeasonInfo)->
    find_family(SeasonInfo#anubis_season_info.seasonrank,Family#family_player.familyid,[]).

find_family([],_FamilyID,_HasCheckSegList)->
    false;
find_family([Seg|RemainSeg],FamilyID,HasCheckSegList)->
    case find_family_in_seg(Seg,FamilyID) of
        false->
            find_family(RemainSeg,FamilyID,[Seg|HasCheckSegList]);
        {SegID,FindOne,Other,SegWithoutFamily}->
            {SegID,FindOne,Other,HasCheckSegList++RemainSeg,SegWithoutFamily}
    end.

find_family_in_seg(#anubis_rank_seg{segnum=FamilyNum,seglist=FamilyList,segid=SegID}=Seg,FamilyID)->
    case FamilyNum >0 of
        true->
            case lists:keytake(FamilyID,#family_player.familyid,FamilyList) of
                {_,FindOne,Other}->
                    case Other of
                        []->
                            SegMin = ?MIN_FAMILY,
                            SegMax = ?MAX_FAMILY,
                            SortOther = [];
                        _ ->
                            SortOther = lists:sort(fun(A,B)->compare_family_player(A,B) end,Other),
                            SegMin = lists:nth(FamilyNum-1,SortOther),
                            SegMax = hd(SortOther)
                    end,
                    {SegID,FindOne,Other,Seg#anubis_rank_seg{seglist=SortOther,segnum=FamilyNum-1,segmin=SegMin,segmax=SegMax}};
                false->
                    false
            end;
        false->
            false
    end.

%%处理赛季切换，赛季奖励会延迟发放，故此处只做状态切换
do_season_transform(State=#state{current_season_id=CurrentSeasonID,transform_season_ref=TRef})->
    case get_anubis_seasoninfo(CurrentSeasonID) of
        ?undefined->
            ?ERR("赛季切换时,当前赛季数据为undefined  State:~w ~n",[State]);
        SeasonInfo->
            %%重新排序
            NewSeasonInfo = balance_seasoninfo(SeasonInfo),
            %%更新当前排行榜数据，添加上准备发送奖励的标志，防止赛季清理的时候删除对应的赛季数据
            set_anubis_seasoninfo(NewSeasonInfo#anubis_season_info{send_reward_flage=?PRE_SEND_REWARD_FLAGE}),
            %%对当前赛季做最后一次同步，防止在赛季切换前的最后一次同步与赛季切换时间内上榜的数据没有同步到完成
            do_syn_rankinfo(CurrentSeasonID,true,CurrentSeasonID),
            %%设置赛季发奖定时器
            RewardDelayInterval = data_anubis:get(season_reward_delay),
            erlang:send_after(RewardDelayInterval*1000,?MODULE,{send_season_reward,CurrentSeasonID})
    end,
    %%修改当前赛季编号
    NewSeasonID = generate_seasonid(CurrentSeasonID),
    NextSeasonInfo = init_anubis_rank_info2(NewSeasonID),
    set_anubis_seasoninfo(NextSeasonInfo),
    
    %%广播赛季切换消息到游戏服anubis_server
    Msg = {update_current_seasonid,NewSeasonID},
    send_msg:broadcast(anubis_server,Msg),
    case data_anubis:get(season_test) of
        false->
            {CYear,CMon,CDay} = erlang:date(),
            {Year,Mon,Day} = get_next_season_begin({CYear,CMon,CDay}),
            TimeStamp = util:datetime_to_seconds({{Year,Mon,Day},{0,0,0}}),
            Interval = TimeStamp - util:now(),
            ?INFO("非测试模式，下赛季距离现在：~w (S)~n",[Interval]);
        true->
            Interval = data_anubis:get(season_interval)
    end,
    Ref = erlang:send_after(Interval*1000,?MODULE,new_season),
    case TRef of ?undefined->ignore;_->catch(erlang:cancel_timer(TRef)) end,
    State#state{current_season_id=NewSeasonID,transform_season_ref=Ref}.

%%处理跨年之后为所有需要删除的赛季信息添加上需要删除的标志
do_delete_all_season(State,DeleteSeasonIDList)->
    ExistSeasonIDList = get_all_exist_seasonIDList(),
    lists:foreach(fun(SeasonID)->
        case lists:member(SeasonID,DeleteSeasonIDList) of
            true->
                case get_anubis_seasoninfo(SeasonID) of
                    ?undefined->
                        ignore;
                    SeasonInfo->
                        set_anubis_seasoninfo(SeasonInfo#anubis_season_info{delete_flage=?PRE_DELETE_SEASON_FLAGE})
                end;
            false->
                ignore
        end
    end,ExistSeasonIDList),
    State.

do_delete_all_season2(#state{current_season_id=CSeasonID}=State)->
    ExistSeasonIDList = get_all_exist_seasonIDList(),
    lists:foreach(fun(SeasonID)->
        case SeasonID =/= CSeasonID of
            true->
                case get_anubis_seasoninfo(SeasonID) of
                    ?undefined->
                        ignore;
                    SeasonInfo->
                        set_anubis_seasoninfo(SeasonInfo#anubis_season_info{delete_flage=?PRE_DELETE_SEASON_FLAGE})
                end;
            false->
                ignore
        end
    end,ExistSeasonIDList),
    State.

do_send_season_reward(SeasonID)->
    case get_anubis_seasoninfo(SeasonID) of
        ?undefined->
            ?ERR("send anubis season:~w  reward no seasoninfo~n",[SeasonID]);
        SeasonInfo->
            NewSeasonInfo1 = balance_seasoninfo(SeasonInfo),
            send_season_reward(NewSeasonInfo1),
            %%截取排行
            NewSeasonInfo = subseason_info(NewSeasonInfo1),
            %%广播截断赛季数据信息
            send_subseason_info(SeasonID),
            %%修改赛季信息的发奖标志，方便赛季清理时能够顺利的删除
            set_anubis_seasoninfo(NewSeasonInfo#anubis_season_info{send_reward_flage=?HAS_SEND_REWARD_FLAGE})
    end.

%%由于游戏服务器只保留了前1000的公会数据，不能自行进行发奖，所以需要排行服驱动对应的游戏服向对应的公会发奖
send_season_reward(SeasonInfo)->
    % ?INFO("send season reward begin~n"),
    #anubis_season_info{seasonrank=SegList,seasonid=SeasonID} = SeasonInfo,
    FamilyPlayerList = lists:foldl(fun(Seg,Acc)->
                Acc++Seg#anubis_rank_seg.seglist
            end,[],SegList),
    SubList = lists:sublist(FamilyPlayerList,data_anubis:get(max_season_reward_num)),
    SendListOrderByServerID = lists:foldl(fun(#family_player{familyserver=ServerID,familyid=FamilyID,familyrank=FamilyRank},Acc)->
        case lists:keytake(ServerID,1,Acc) of
            false->
                [{ServerID,[{FamilyID,FamilyRank}]}|Acc];
            {_Value,{ServerID,ServerSendAcc},Other}->
                [{ServerID,[{FamilyID,FamilyRank}|ServerSendAcc]}|Other]
        end
    end,[],SubList),
    % ?ERR("FamilyPlayerList:~w SubList:~w ~n",[FamilyPlayerList,SubList]),
    lists:foreach(fun(SingleServerReward)->
        {DesServerID,FamilyList} = SingleServerReward,
        Msg = {send_season_reward,FamilyList,SeasonID},
        % ?ERR("Msg:~w ~n",[Msg]),
        send_msg:direct(DesServerID,anubis_server,Msg)
    end,SendListOrderByServerID).

%%旧的数据一般情况下不会用于插入等操作，直接保留一个seg即可
subseason_info(SeasonInfo)->
    #anubis_season_info{seasonrank=SegList,rankfamilynum=FamilyNum} = SeasonInfo,
    SubLength = data_anubis:get(max_history_store_rank_num),
    FamilyList = lists:foldl(fun(Seg,Acc)->Acc++Seg#anubis_rank_seg.seglist end,[],sort_seg(SegList)),
    % FamilyLength = length(FamilyList),
    {NewSeg,NewNum}= case FamilyNum=<SubLength of
        true->
            case FamilyNum=:= 0 of
                false->
                    {#anubis_rank_seg{segid=1,segmin=lists:nth(FamilyNum,FamilyList),segmax=hd(FamilyList),segnum=FamilyNum,seglist=FamilyList},FamilyNum};
                true->
                    {#anubis_rank_seg{segid=1,segmin=?MIN_FAMILY,segmax=?MAX_FAMILY,segnum=FamilyNum,seglist=[]},FamilyNum}
            end;
        false->
            SubFamilyList = lists:sublist(FamilyList,1,SubLength),
            {#anubis_rank_seg{segid=1,segmin=lists:nth(SubLength,SubFamilyList),segmax=hd(SubFamilyList),segnum=SubLength,seglist=SubFamilyList},SubLength}
    end,
    SeasonInfo#anubis_season_info{seasonrank=[NewSeg],rankfamilynum=NewNum}.        

%%数据持久化,此处可能需要转换格式
do_persist(State)->
    {_Dictionary,DicList} = erlang:process_info(self(),dictionary),
    SeasonInfoList = [SeasonInfo||{{?SEASONINFO,_},SeasonInfo}<-DicList],
    db_sql:set_etc(?DB_ETC_KEY_ANUBIS_RANK_DATE,{State,SeasonInfoList}).

get_next_seasonid(PreID)->
    {_Year,Mon,_Day} = erlang:date(),
    case Mon =:= 1 of
        true->
            1;
        false->
            case PreID +1 > 12 of 
                true->
                    ?INFO("赛季编号异常:SeasonID：~w 当前月：~w ~n",[PreID,Mon]),
                    Mon;
                false->
                    PreID+1
            end
    end.
generate_seasonid()->
    {Year,_Mon,_Day} = erlang:date(),
    Year*100+1.
generate_seasonid(PreSeasonID)->
    {CYear,_CMon,_CDay} = erlang:date(),
    generate_seasonid2(PreSeasonID,CYear).

generate_seasonid2(PreSeasonID,CYear)->
    PYear = PreSeasonID div 100,
    PMon = PreSeasonID rem 100,
    case PYear=:=CYear of
        true->
            PYear*100+PMon+1;
        false->
            CYear*100+1
    end.

get_next_season_begin({Year,Mon,_Day})->
    {NewYear,NewMon} = case Mon +1 > 12 of
        true->
            {Year+1,1};
        false->
            {Year,Mon+1}
    end,
    {NewYear,NewMon,1}.

get_seg_num(Max_rank_num,Max_show_rank_num)->
    case (Max_rank_num rem Max_show_rank_num) >0 of
        true->
            Max_rank_num div Max_show_rank_num +1;
        false ->
            Max_rank_num div Max_show_rank_num
    end. 
balance_seasoninfo(SeasonInfo)->
    #anubis_season_info{seasonrank=SegList}=SeasonInfo,
    {NewSegList,NewNum} = balance_seg(sort_seg(SegList)),
    SeasonInfo#anubis_season_info{seasonrank=sort_seg(NewSegList),rankfamilynum=NewNum}.

% -record(anubis_rank_seg,{segid=0,segmin=?MIN_FAMILY,segmax=?MAX_FAMILY,segnum=0,seglist=[]}).
balance_seg(SegList)->
    FamilyList1 = lists:foldl(fun(Seg,Acc)->
        Acc++Seg#anubis_rank_seg.seglist
    end,[],SegList),
    FamilyList2 = lists:sort(fun(A,B)->compare_family_player(A,B) end,FamilyList1),
    FamilyList = lists:sublist(FamilyList2,data_anubis:get(max_store_rank_num)),
    MaxFamilyListNum = length(FamilyList),
    SegNum = get_seg_num(data_anubis:get(max_store_rank_num),data_anubis:get(max_seg_length)),
    Result = lists:foldl(fun(SegID,Acc)->
        case get_seg_ranklist(SegID,FamilyList) of
            []->
                [#anubis_rank_seg{segid=SegID,seglist=[],segmin=?MIN_FAMILY,segmax=?MAX_FAMILY,segnum=0}|Acc];
            NewPlayerList->
                [#anubis_rank_seg{segid=SegID,seglist=NewPlayerList,segmin=lists:nth(length(NewPlayerList),NewPlayerList),segmax=hd(NewPlayerList),segnum=length(NewPlayerList)}|Acc]
        end
    end,[],lists:seq(1,SegNum)), 
    {Result,MaxFamilyListNum}.

get_seg_ranklist(SegID,FamilyList)->
    AllLength = length(FamilyList),
    ShowNum = data_anubis:get(max_seg_length),
    Begin = (SegID-1)*ShowNum+1,
    End = SegID * ShowNum,
    List = if
        Begin =< AllLength andalso End =< AllLength ->
            lists:sublist(FamilyList,Begin,ShowNum);
        Begin =< AllLength andalso End > AllLength->
            lists:sublist(FamilyList,Begin,AllLength-Begin+1);
        true->
            []
    end,
    {ListWithRank,_Index} = lists:foldl(fun(E,{Acc,IndexAcc})->
        NE=E#family_player{familyrank=IndexAcc},
        % ?ERR("OldRank:~w Balance Rank:~w~n",[E#family_player.familyrank,NE#family_player.familyrank]),
        % case E#family_player.familyrank=/=NE#family_player.familyrank of
        %     true->
        %         {[NE#family_player{ischange=?FAMILY_CHANGE}|Acc],IndexAcc+1};
        %     false->
        %         {[NE|Acc],IndexAcc+1}
        % end
        {[NE|Acc],IndexAcc+1}
    end,{[],Begin},List),
    lists:reverse(ListWithRank).

get_familylist(SeasonID)->
    case get_anubis_seasoninfo(SeasonID) of
        ?undefined->
            ?undefined;
        #anubis_season_info{seasonrank=SegList}->
            lists:foldl(fun(Seg,Acc)->
                Acc++Seg#anubis_rank_seg.seglist
            end,[],SegList)
    end.

%%同步排名数据到ServerID游戏服
do_syn_rankinfo(SeasonID,ServerID,Force,CSeasonID)->
    {_,SeasonID,L,Force,_}=SynMsg = generate_syn_msg(SeasonID,Force,CSeasonID),
    case L of
        []->
            ignore;
        _ ->
            ?INFO("SynMsg SeasonID:~w length:~w False:~w  ~n",[SeasonID,length(L),Force]),
            send_msg:direct(ServerID,anubis_server,SynMsg)
    end.

%%广播排名数据到所有的游戏服
do_syn_rankinfo(SeasonID,Force,CSeasonID)->
    {_,SeasonID,L,Force,_}=SynMsg = generate_syn_msg(SeasonID,Force,CSeasonID),
    case L of
        []->
            ignore;
        _ ->
            ?INFO("SynMsg SeasonID:~w length:~w False:~w  ~n",[SeasonID,length(L),Force]),
            send_msg:broadcast(anubis_server,SynMsg)
    end.

%%将阶段赛季数据的数据广播到所有游戏服,通知游戏服将对应赛季的数据截断
send_subseason_info(SeasonID)->
    Msg = {subseason_info,SeasonID},
    send_msg:broadcast(anubis_server,Msg).

generate_syn_msg(SeasonID,Force,CSeasonID)->
    case get_anubis_seasoninfo(SeasonID) of
        ?undefined->
            {syn_anubis_rank,SeasonID,[],Force,false};
        #anubis_season_info{seasonrank=SegList}=SeasonInfo->
            {NewSegList,FamilyList,_NeedSynNum} = generate_syn_msg2(sort_seg(SegList),Force,CSeasonID,SeasonID),
            NewSeasonInfo = SeasonInfo#anubis_season_info{seasonrank=sort_seg(NewSegList)},
            %%同步数据之前将所有的公会的Rank值更新成正确的值
            NewSeasonInfo1=anubis_rank_server:balance_seasoninfo(NewSeasonInfo),
            set_anubis_seasoninfo(NewSeasonInfo1),
            {syn_anubis_rank,SeasonID,FamilyList,Force,true}
    end.

%%此处应该只同步前端需要显示的排行数据长度即可，避免多余的数据的同步
generate_syn_msg2(RankSegList,true,CSeasonID,SynSeasonID)->
    SynListLength = get_syn_rank_length(SynSeasonID,CSeasonID),
    lists:foldl(fun(#anubis_rank_seg{seglist=FamilyList,segnum=FamilyNum}=Seg,{SegAcc,Acc,NeedSynNum})->
        case NeedSynNum > 0 of
            false->
                %%不需要再增加同步的数据
                {[Seg|SegAcc],Acc,0};
            true->
                case NeedSynNum >= FamilyNum of
                    true->
                        {[Seg|SegAcc],Acc++FamilyList,NeedSynNum-FamilyNum};
                    false->
                        {[Seg|SegAcc],Acc++lists:sublist(FamilyList,1,NeedSynNum),0}
                end
        end
    end,{[],[],SynListLength},RankSegList);
generate_syn_msg2(RankSegList,false,CSeasonID,SynSeasonID)->
    SynListLength = get_syn_rank_length(SynSeasonID,CSeasonID),
    lists:foldl(fun(#anubis_rank_seg{seglist=FamilyList}=Seg,{SegAcc,Acc,NeedSynNum})-> 
        case NeedSynNum > 0 of
            false->
                {[Seg|SegAcc],Acc,0};
            true->
                {NewFamilyList,SynList,RemainNeedSynNum} = lists:foldl(fun(F,{FamilyAcc,SynAcc,NeedSynNumAcc})->
                    case NeedSynNumAcc > 0 of
                        false->
                            {[F|FamilyAcc],SynAcc,NeedSynNum};
                        true->
                            case F#family_player.ischange=:=?FAMILY_CHANGE of 
                                true->
                                    {[F#family_player{ischange=?FAMILY_NOCHANGE}|FamilyAcc],[F|SynAcc],NeedSynNumAcc-1};
                                false->
                                    {[F|FamilyAcc],SynAcc,NeedSynNumAcc-1}
                            end
                    end
                end,{[],[],NeedSynNum},FamilyList),
                {[Seg#anubis_rank_seg{seglist=lists:reverse(NewFamilyList)}|SegAcc],Acc++lists:reverse(SynList),RemainNeedSynNum}
        end 
    end,{[],[],SynListLength},RankSegList).

get_syn_rank_length(SynSeasonID,CSeasonID)->
    case SynSeasonID=:=CSeasonID of
        true->
            data_anubis:get(max_store_rank_num);
        false->
            data_anubis:get(max_history_store_rank_num)
    end.

%%按照战斗结束的时间作为所属赛季的判断标准，防止卡赛季的情况
do_anubis_fight_result(FamilyPlayer,FightMsg,SeasonID,CSeasonID)->
    % {AddFightScore,AddKillNum,_HonorNum,_UnionCoinNum,_RewardList}=calculate_fight_score(FightMsg),
    #anubis_fight_result{fightresult=FightResult,killnum=TotalKillNum,resource=TotalResource} = FightMsg,
    AddFightScore = calculate_family_fight_score(FightResult,TotalKillNum,TotalResource),
    #family_player{familyscore=OldFamilyScore,familykillnum=OldFamilyKillNum,familyserver=ServerID}=FamilyPlayer,
    if 
        SeasonID < CSeasonID ->
            %%战斗过程中发生赛季的切换
            CSeasonFamilyPlayer = FamilyPlayer#family_player{familyscore=AddFightScore,familykillnum=TotalKillNum,ischange=?FAMILY_CHANGE};
        SeasonID =:= CSeasonID ->
            CSeasonFamilyPlayer = FamilyPlayer#family_player{familyscore=OldFamilyScore+AddFightScore,familykillnum=OldFamilyKillNum+TotalKillNum,ischange=?FAMILY_CHANGE};
        true->
            ?INFO("anubis_server seasonid:~w  big than anubis_rank_server seasonid:~w FamilyPlayer:~w FightMsg:~w ~n",[SeasonID,CSeasonID,FamilyPlayer,FightMsg]),
            CSeasonFamilyPlayer = FamilyPlayer#family_player{familyscore=AddFightScore,familykillnum=TotalKillNum,ischange=?FAMILY_CHANGE}
    end,
    %%将当前赛季的公会数据更新到当前赛季排行中
    anubis_rank_server:update_family_info(CSeasonFamilyPlayer,CSeasonID),
    %%将当前赛季的公会数据更新到对应游戏公会中,并且包含战斗结果数据，方便发放本次战斗奖励
    %%插入返回的排名只能表示当前时间的排名，随着其他的插入会变化，故不直接更新公会的排名
    UpdateFamilyInfoMsg = {update_family_anubis_info_after_fight,CSeasonID,CSeasonFamilyPlayer,FightMsg},
    send_msg:direct(ServerID,anubis_server,UpdateFamilyInfoMsg).

calculate_family_fight_score(FightResult,TotalKillNum,TotalResource)->
    case TotalResource=:= 0 andalso TotalKillNum=:=0 andalso FightResult=/=test of
        true->
            RealFightResult = lose;
        false->
            RealFightResult = FightResult
    end,
    FightScoreBase = case data_anubis:get({score,RealFightResult}) of ?undefined->0;SX->SX end,
    erlang:min(erlang:max(FightScoreBase + TotalKillNum,0),data_anubis:get(score_limit)).

calculate_family_member_score(FightResult,KillNum,TotalResource,TotalKillNum)->
    FightScore = calculate_family_fight_score(FightResult,TotalKillNum,TotalResource),
    UnionCoin = trunc(FightScore*data_anubis:get(score_transform_rate)),
    ResourceHonor = trunc(TotalResource*data_anubis:get(resource_transform_rate)),
    RealFightResult = case TotalResource=:=0 andalso TotalKillNum=:= o andalso FightResult=/=test of true-> lose;false->FightResult end,
    HonorBaseValue = case data_anubis:get({honor,RealFightResult}) of ?undefined->0;HX->HX end,
    KillHonor = trunc(KillNum*data_anubis:get(kill_one)),
    Honor = erlang:min(ResourceHonor+HonorBaseValue+KillHonor,data_anubis:get(honor_limit)),
    Reward = #sell_reward{item=[#new_item{itemTypeID=?ITEM_TYPE_HONOR,itemNum=Honor,itemLevel=1,itemRank=0},#new_item{itemTypeID=?ITEM_TYPE_UNION_COIN,itemNum=UnionCoin,itemLevel=1,itemRank=0}]},
    {FightScore,Honor,UnionCoin,Reward}.

%%此处需要根据实际情况修改
send_to_me(Msg) ->
    send_msg:direct_by_name(family_cross_master, anubis_rank_server, Msg).

%%==============================================================================
%%战斗完成之后调用此接口上榜,此处的FamilyPlayer是战斗战斗开始前的数据,FightMsg是战斗的结果，用于评判该场战斗的积分,SeasonID为战斗前获取的赛季号，从anubis_server获取
up_anubis_rank(FamilyPlayer,FightMsg,SeasonID) when is_record(FamilyPlayer,family_player)->
    Msg = {anubis_fight_result,FamilyPlayer,FightMsg,SeasonID},
    anubis_rank_server:send_to_me(Msg);
up_anubis_rank(FamilyPlayer,_FightMsg,_SeasonID)->
    ?ERR("not family_player format:~w ~n",[FamilyPlayer]).

%%获取当前存在的赛季编号，方便游戏服重启之后能够及时请求需要的赛季数据,将会异步的方式返回结果，需要接收对应的return消息{get_exist_season_id_return,SeasonIDList}
get_exist_season_id_list(ServerID)->
    Msg = {get_exist_season_id,ServerID},
    anubis_rank_server:send_to_me(Msg).

%%请求同步对应赛季的数据排名,全部同步，不进行增量同步，主要用于游戏服重启之后主动请求
request_syn_seasoninfo(SeasonID,ServerID)->
    request_syn_seasoninfo(SeasonID,ServerID,true).

%%请求同步对应赛季的数据排名，可以进行增量同步,一般不需要调用
request_syn_seasoninfo(SeasonID,ServerID,Force)->
    Msg = {syn_rankinfo,SeasonID,ServerID,Force},
    anubis_rank_server:send_to_me(Msg).

sort_family_member(FamilyMemberList,SeasonID)->
    SortMemberList = lists:sort(fun(A,B)->compare_family_member(A,B,SeasonID) end,FamilyMemberList),
    {SortMemberWithRankList,_IndexAcc} = lists:foldl(fun(E,{Acc,Index})->
            {[{Index,E}|Acc],Index+1}
    end,
    {[],1},SortMemberList),
    lists:reverse(SortMemberWithRankList).

%%比较公会内，成员之间赛季SeasonID阿努比斯表现排名(比较顺序阿努比斯表现，公会职位，等级，战斗力)
compare_family_member(A,B,SeasonID)->
    #family_member_info{anubisinfo=AA,family_title=TitleA,fight_power=FightPowerA,role_level=LevelA} = A,
    #family_member_info{anubisinfo=AB,family_title=TitleB,fight_power=FightPowerB,role_level=LevelB} = B,
    SeasonAA = family_server:find_anubis_member_season(AA,SeasonID),
    SeasonAB = family_server:find_anubis_member_season(AB,SeasonID),
    case compare_family_member_season(SeasonAA,SeasonAB) of
        equal->
            case family_server:compare_member_family_title(TitleA,TitleB) of
                equal->
                    (LevelA>LevelB) orelse ((LevelA=:=LevelB) andalso (FightPowerA>FightPowerB));
                TitleResult->
                    TitleResult
            end;
        AnubisResult->
            AnubisResult
    end.

%%比较具体赛季的阿努比斯表现
% -record(anubis_representation,{seasonid=0,killnum=0,resource=0}).
compare_family_member_season(?undefined,?undefined)->
    equal;
compare_family_member_season(?undefined,_SeasonAB)->
    false;
compare_family_member_season(_SeasonAA,?undefined)->
    true;
compare_family_member_season(SeasonAA,SeasonAB)->
    #anubis_member_representation{killnum=KillNumA,resourcepoint=ResourcePointA}=SeasonAA,
    #anubis_member_representation{killnum=KillNumB,resourcepoint=ResourcePointB}=SeasonAB,
    if
        (KillNumA>KillNumB) orelse (KillNumA=:=KillNumB andalso ResourcePointA> ResourcePointB)->
            true;
        (KillNumA=:=KillNumB andalso ResourcePointA=:=ResourcePointB)->
            equal;
        true->
            false
    end.

get_all_exist_seasonIDList()->
    {_Dictionary,DicList} = erlang:process_info(self(),dictionary),
    [SeasonID||{{?SEASONINFO,SeasonID},_SeasonInfo}<-DicList].

delete_invalid_seasoninfo()->
    ExistSeasonIDList = get_all_exist_seasonIDList(),
    DeleteSeasonIDList = lists:foldl(fun(SeasonID,Acc)->
        case get_anubis_seasoninfo(SeasonID) of
            ?undefined->
                Acc;
            #anubis_season_info{delete_flage=DeleteFlage,send_reward_flage=SendRewardFlage}->
                case DeleteFlage=:=?PRE_DELETE_SEASON_FLAGE andalso SendRewardFlage=:=?HAS_SEND_REWARD_FLAGE of
                    true->
                        % ?ERR("delete SeasonID:~w ~n",[SeasonID]), 
                        erlang:erase({?SEASONINFO,SeasonID}),
                        [SeasonID|Acc];
                    false->
                        Acc
                end
        end
    end,[],ExistSeasonIDList),
    case DeleteSeasonIDList =/=[] of
        true->
            Msg = {delete_season_rank,DeleteSeasonIDList},
            send_msg:broadcast(anubis_server,Msg);
        false->
            ignore
    end.
%%=======================测试用辅助代码==========================
test_get_result()->
    Rand = random:uniform(),
    if 
        Rand < 0.34 ->
            win;
        Rand < 0.67 ->
            equal;
        true->
            lose
    end.
generate_random_fightmsg()->
    #anubis_fight_result{fightresult=test_get_result(),killnum=trunc(random:uniform()*100),resource=trunc(random:uniform()*2000)}.

do_test_balance_season(SeasonID)->
    case get_anubis_seasoninfo(SeasonID) of
        ?undefined->
            io:format("can not find seasoninfo:~w ~n",[SeasonID]);
        SeasonInfo->
            NewSeasonInfo=balance_seasoninfo(SeasonInfo),
            set_anubis_seasoninfo(NewSeasonInfo)
    end.
    
%%=======================测试命令===============================
%%改命令用于对对应赛季数据重新平衡
test_season_balance(SeasonID)->
    Msg = {test_balance_season,SeasonID},
    anubis_rank_server:send_to_me(Msg).

%%该命令发送对应赛季的奖励
test_send_reward(SeasonID)->
    Msg = {send_season_reward,SeasonID},
    anubis_rank_server:send_to_me(Msg).

%%该命令在游戏服执行，同步当前服务器的公会信息到排行榜中，测试使用
test(SeasonID)->
    ServerID = data_setting:get(server_id),
    Sql = io_lib:format("select familyID from gFamily;",[]),
    {ok,FamilyIDList} = db_sql:sql_execute_with_log2(Sql),
    FamilyInfoList = [db_sql:get_family_info(FamilyID)||[FamilyID]<-FamilyIDList],
    PFamilyInfoList = [family_misc:to_p_family_info(FamilyInfo)||FamilyInfo<-FamilyInfoList],
    FamilyPlayerAndFightMsgList = lists:foldl(fun(PFamilyInfo,Acc)->
            #p_family_info{family_id=FamilyID,family_name=FamilyName,owner_role_name=OwnerRoleName,total_fight_power =FamilyFightPower,level=FamilyLevel}=PFamilyInfo,
            FamilyPlayer=#family_player{familyid=FamilyID,familyname=FamilyName,familyserver=ServerID,familyleader=OwnerRoleName,familyfightpower=FamilyFightPower,familylevel=FamilyLevel},
            FightMsg = generate_random_fightmsg(),
            [{FamilyPlayer,FightMsg}|Acc]
    end,[],PFamilyInfoList),
    lists:foreach(fun({FP,FR})->up_anubis_rank(FP,FR,SeasonID) end,FamilyPlayerAndFightMsgList).

%%随机生成N个公会数据到排行榜中
test(N,SeasonID)->
    BeginID = data_setting:get(server_id)*100000,
    MsgList = [{#family_player{familyid=ID,familyname=list_to_binary("family"++integer_to_list(ID)),familyserver=0,familyscore=trunc(random:uniform()*100),familykillnum=trunc(random:uniform()*50),familyfightpower=trunc(random:uniform()*1000000),familyleader=list_to_binary("leader"++integer_to_list(ID)),familylevel=trunc(random:uniform()*10)},generate_random_fightmsg()}||ID<-lists:seq(BeginID,BeginID+N)],
    lists:foreach(fun({FP,FR})->up_anubis_rank(FP,FR,SeasonID) end,MsgList).

i()->
    gen_server:call(?MODULE,i).

test_syn_rankinfo(SeasonID,Force)->
    Msg = {test_syn_rankinfo,SeasonID,Force},
    anubis_rank_server:send_to_me(Msg).