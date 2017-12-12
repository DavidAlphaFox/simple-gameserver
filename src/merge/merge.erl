%%%-------------------------------------------------------------------
%%% @author dongquan <dongquan360@gmail.com>
%%% @copyright (C) 2013, crimoon
%%% @doc
%%% 合服相关代码
%%% @end
%%%-------------------------------------------------------------------
-module(merge).

-compile(export_all).

-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_activity_rank.hrl").

%% API
-export([
        start/0,
        spawn_start/0
    ]).

-define(APP, [crypto,emysql,logger]).
-define(LOG(Format, Args), merge_log:sendto_log_server({log, ?MODULE, ?LINE, erlang:localtime(), Format, Args})).
-define(NEED_DELETE_ETC,[
                            ?DB_ETC_KEY_PLUNDER_ROBOT,  %% 碎片争夺战机器人
                            ?DB_ETC_KEY_EMPEROR,
                            %%?DB_ETC_KEY_FIRE,           %% 神殿祈福(爆竹) 全局数据
                            %%?DB_ETC_KEY_FIRE_ROLEINFO,  %% 神殿祈福(爆竹) 玩家数据
                            ?DB_ETC_KEY_TALK,
                            ?DB_ETC_KEY_EIGHT_REPLAY
                        ]).
%% 当数据比较大时,合并时采用分页的方式加载和合并
-define(SELECTLIMIT, 5000).

%% 每次写入的数据个数
-define(WRITELIMIT, 300).

%% 每次删除时的数据个数
-define(DELETELIMIT, 500).

%% 放爆竹服务器信息
-record(d_fire_info,{total                  %% 燃放总数
                     ,rankList              %% 排名列表 [{roleID, count}, {roleID, count}]
                     ,rewardList            %% 已经领奖的列表
                    }).

execute(PoolID, Sql) ->
    case catch emysql:execute(PoolID, Sql) of
        {ok_packet, A,B,C,D,E,F} ->
            {ok_packet, A,B,C,D,E,F};
        {result_packet, A, B, C, D} ->
            {result_packet, A, B, C, D};
        {'EXIT',{connection_down,{and_conn_reset_failed,{cannot_reopen_in_reset,{failed_to_connect_to_database,econnrefused}}}}} ->
            re_add_pool(),
            execute(PoolID, Sql);
        Exception ->
            io:format("异常:~w,Sql:~s,~n", [Exception, lists:sublist(Sql, 100)])
%%             ?LOG("异常:~w,Sql:~s,~n", [Exception, Sql]),
%%             timer:sleep(10000),
%%             execute(PoolID, Sql)
    end.

%% 如果mysql服务挂掉，需要先remove_pool，再add_pool,在这之前可以加一个重启mysql服务的命令或者手动重启
re_add_pool() ->
    case os:type() of
        {win32,nt} ->
            to_do_os_cmd;
        {unix, linux} ->
            to_do_os_cmd
    end,
    ServerIDList = do_merge:get(server_id_list),
    MasterServerID = data_setting:get(server_id),
    AllServerIDList = [MasterServerID|ServerIDList],
    ok = remove_pool(MasterServerID, AllServerIDList),
    re_add_pool(MasterServerID, AllServerIDList).

re_add_pool(_MasterServerID, []) ->
    ok;
re_add_pool(MasterServerID, AllServerIDList) ->
    case AllServerIDList of
        [MasterServerID|LeftServerIDList] ->
            PoolID = get_master_pool_id(),
            {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database);
        [SlaveServerID|LeftServerIDList] ->
            PoolID = get_slave_pool_id(SlaveServerID),
            {IP,Port,Name,Pass,DBName,Num} = do_merge:get({database, SlaveServerID})
    end,
    case catch emysql:add_pool(PoolID, Num,Name,Pass, IP, Port,DBName, utf8) of
        ok ->
            re_add_pool(MasterServerID, LeftServerIDList);
        {'EXIT',pool_already_exists} ->
            re_add_pool(MasterServerID, LeftServerIDList);
        {failed_to_connect_to_database,econnrefused} ->
            re_add_pool();
        Err ->
            ?LOG("~w~n", [Err]),
            timer:sleep(10000),
            re_add_pool(MasterServerID, AllServerIDList)
    end.

%% remove_pool的操作
remove_pool(_MasterServerID, []) ->
    ok;
remove_pool(MasterServerID, AllServerIDList) ->
    case AllServerIDList of
        [MasterServerID|LeftServerIDList] ->
            PoolID = get_master_pool_id();
        [SlaveServerID|LeftServerIDList] ->
            PoolID = get_slave_pool_id(SlaveServerID)
    end,
    case catch emysql:remove_pool(PoolID) of
        ok ->
            remove_pool(MasterServerID, LeftServerIDList);
        {'EXIT',pool_not_found} ->
            remove_pool(MasterServerID, LeftServerIDList);
        Err ->
            ?LOG("~w~n", [Err]),
            timer:sleep(1000),
            remove_pool(MasterServerID, AllServerIDList)
    end.


%% 分段批量插入
make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum) ->
    make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, 0, "").

make_sql_batch_by_piece(PoolID, Sql, _Format, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
            ignore;
        true ->
            Sql2 = Sql ++ tl(Acc),
            {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2)
    end;
make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ tl(Acc),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2),
    make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, 0, "");
make_sql_batch_by_piece(PoolID, Sql, Format, [E|List], PieceNum, AccNum, Acc) ->
    Acc2 = ","++io_lib:format(Format,E)++Acc,
    make_sql_batch_by_piece(PoolID, Sql, Format, List, PieceNum, AccNum+1, Acc2).

%%指定匹配值分段批量删除
make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum) ->
    make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, 0, "").

make_sql_batch_del_by_piece(PoolID, Sql, [], _PieceNum, _AccNum, Acc) ->
    if Acc == "" ->
            ignore;
        true ->
            Sql2 = Sql ++ "(" ++ Acc,
            {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2)
    end;
make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, PieceNum, Acc) ->
    Sql2 = Sql ++ "(" ++ Acc,
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql2),
    make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, 0, "");
make_sql_batch_del_by_piece(PoolID, Sql, [E|List], PieceNum, AccNum, Acc) ->
    case AccNum of
        0 ->
            Acc2 = erlang:integer_to_list(E) ++ ");";
        _ ->
            Acc2 = erlang:integer_to_list(E) ++ "," ++ Acc
    end,
    make_sql_batch_del_by_piece(PoolID, Sql, List, PieceNum, AccNum+1, Acc2).

%%%===================================================================
%%% API
%%%===================================================================
spawn_start() ->
    erlang:spawn(fun() -> merge:start() end).

start() ->
    erlang:register(?MODULE, self()),
    start_init(),
    ?LOG("~ts~n", ["数据库连接成功"]),
    do_start_merge(),
    ?LOG("~ts~n", ["合服完毕，恭喜！"]),
    tk_misc:stop_applications(?APP).

%% 不合服，只是清理无效的帐号数据
start_clear_junk() ->
    erlang:register(?MODULE, self()),
    start_init(),
    ?LOG("~ts~n", ["数据库连接成功"]),
    do_delete_dead_player([], true),
    ?LOG("~ts~n", ["无效帐号清理成功"]),
    tk_misc:stop_applications(?APP).

get_master_pool_id() ->
    master_pool_id.

get_slave_pool_id(ServerID) when erlang:is_integer(ServerID) ->
    erlang:list_to_atom(lists:append("slave_pool_id_", erlang:integer_to_list(ServerID))).

start_init() ->
    %% 启动emysql
    tk_misc:start_applications(?APP),
    tk_config:reload_config("config/do_merge.config",do_merge,key_value,original),
    %% 启动日志服务器
    do_start_log_server(),
    ?LOG("~ts~n", ["主节点：读取合服配置文件成功，日志服务器启动成功，准备连接mysql"]),
    {IP,Port,Name,Pass,DBName,Num} = data_setting:get(database),
    ok = emysql:add_pool(get_master_pool_id(), Num,Name,Pass, IP, Port,DBName, utf8),
    ServerIDList = do_merge:get(server_id_list),
    ?LOG("~ts:~w~n", ["主节点mysql连接成功，准备连接从服务器的mysql，本次参与合服的区包括", ServerIDList]),
    lists:foreach(
        fun(ServerID) ->
                {SlaveIP,SlavePort,SlaveName,SlavePass,SlaveDBName,SlaveNum} = do_merge:get({database, ServerID}),
                ok = emysql:add_pool(get_slave_pool_id(ServerID), SlaveNum,SlaveName,SlavePass, SlaveIP, SlavePort,SlaveDBName, utf8)
        end, ServerIDList).

%% 开始合并数据
do_start_merge() ->
    ServerIDList = do_merge:get(server_id_list),
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                case ServerID =< MasterServerID of
                    true ->
                        ?LOG("~ts~n", ["配置错误，主服务器的ID不是最小的,请检查配置"]),
                        erlang:throw(config_error);
                    false ->
                        next
                end
        end, ServerIDList),
    ?LOG("~ts~n", ["先删除所有的战报、汉帝宝库数据、聊天数据"]),
    do_delete_tables(ServerIDList),

    ?LOG("~ts~n", ["删除死号数据，包括机器人的数据"]),
    do_delete_dead_player(ServerIDList, false),

    ?LOG("~ts~n", ["准备处理重名"]),
    do_process_duplicate_name(ServerIDList),

    ?LOG("~ts~n", ["准备处理重复的gpay主键"]),
    do_process_gpay(ServerIDList),

    %% 如果一张表,不保留的数据超过百分之70～90的话,可以先筛选出保留的部分
    %% 然后truncate table掉整个表,之后再回写。在需要删除的数据量很大
    %% 时这种效率应该会更好。目前只有gMail一个表需要处理,此表过大，单独数据库操作处理
%%     do_filter_merge(ServerIDList),

    ?LOG("~ts~n", ["账号处理完毕，准备 合并数据"]),
    %% 开始遍历各种数据并插入到最终数据表中
    do_merge_data(ServerIDList),

    ?LOG("~ts~n", ["数据合并完毕，开始更新活动数据"]),
    do_process_activity(ServerIDList),

    %% 生成一个文件作为合服的临时标识
    file:write_file("./merge.touch", [], [append, delayed_write]),
    ?LOG("~ts~n", ["合服完毕，恭喜！"]),
    ok.

do_process_activity(ServerIDList) ->
    %% is_doing_merge在生成帐号时用到
    %% ETS_ID在生成各种ID时用到
    %% roleName在生成名字时用到
    erlang:put(?is_doing_merge, true),
    ets:new(roleName, [public, set, named_table]),
    ets:new(?ETS_ID, [{keypos, 1}, set, public, named_table]),
    init_id(gerID, "gerID","gGer",?GER_ID_BASE),
    ItemID1 = init_id2("itemUID", "gBagItem", ?ITEM_ID_BASE),
    ItemID2 = init_id2("itemUID", "gEquip", ?ITEM_ID_BASE),
    ItemID3 = erlang:max(ItemID1,ItemID2),
    ets:insert(?ETS_ID, {itemUID,ItemID3}),
    MasterPoolID = get_master_pool_id(),
    {result_packet, _, _, RoleNameList,_} = execute(MasterPoolID, "select `roleName` from gRole"),
    lists:foreach(fun([RoleName]) ->
                    ets:insert(roleName, {erlang:binary_to_list(RoleName), 1})
                 end, RoleNameList),
    
    %% 合并家园任务。家园任务id只是服务器内唯一，需要对合并的id做处理，以插入方式加入新库
    lists:foreach(fun(SID) ->
            SlavePoolID = get_slave_pool_id(SID),
            {result_packet, _,_, Rows, _} = execute(SlavePoolID, "select * from ghometask;"),
            sync_home_task2(MasterPoolID,Rows)
        end, ServerIDList),
        
    %%添加加机器人
    ?LOG("开始添加机器人.~n", []),
    {ok, RobotList} = gen_robot_data(MasterPoolID),
    {InsertSql, Format, NewDataList} = db_trans:trans(gRole, RobotList),
    make_sql_batch_by_piece(MasterPoolID, InsertSql, Format, NewDataList, ?WRITELIMIT),
    ?LOG("机器人添加成功.~n", []),
    ?LOG("清理ETC数据.~n", []),
    lists:foreach(fun(E) -> {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, io_lib:format("delete from gETC where `key` = ~w;",[E])) end, ?NEED_DELETE_ETC),

    ?LOG("处理需要合并子区的ETC数据.~n", []),
    do_process_etc(ServerIDList),

    %%重置家园充能、基因融合
    {EnergyTimes,_,_,_} = data_homestead:get(init_add_energy_data),
    HomesteadSql = io_lib:format("UPDATE `gHomestead` SET `addEnergyTimes` = ~w, `matingTimes` = 0, `refreshMatingSecond`=0;", [EnergyTimes]),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, HomesteadSql),
    %% 合并新年红包的活动数据，数据结构特殊，这里单独处理
    merge_etc_data(),
    
    merge_treaHouse(ServerIDList),  %% 汉帝宝库(妖精宝藏)
    merge_fire(MasterPoolID,ServerIDList),       %% 神殿祈福(爆竹)
    
    ok.

merge_etc_data() ->
    MasterPoolID = get_master_pool_id(),
    ServerIDList = do_merge:get(server_id_list),
    SlavePoolIDList = lists:map(fun(ServerID) -> get_slave_pool_id(ServerID) end, ServerIDList),
    merge_rebate(MasterPoolID, SlavePoolIDList).

merge_treaHouse(ServerIDList)->
    ?ETS_TABLE_NAME = ets:new(?ETS_TABLE_NAME, [set, protected, named_table, {keypos, 1}, {heir, none}, {write_concurrency,false}, {read_concurrency,true}]),
    {TreaHouseState, TreaHouseEts}=
        case get_etc(get_master_pool_id(),?DB_ETC_KEY_ACTIVITYRANK) of
            [] ->
                {{state},[]};
            {A,B}->
                {A,B}
        end,
    lists:foreach(fun(Object)->
                    ets:insert(?ETS_TABLE_NAME, Object)
                  end, TreaHouseEts),
%%     TreaHouseState = activityRank_server:init_activity_info(),
    ActivityID = activityRank_server:get_treaHouse_activityID(),
    Now = util:now(),
    case data_activityRank:get(ActivityID) of
        %% 只能在主服处于活动开启且未结算期间才能合服
        #data_activity_rank{startTime=StartTime,stopTime=StopTime} = ActivityInfo ->
            StartTimeSecond = util:datetime_to_seconds(StartTime),
            StopTimeSecond = util:datetime_to_seconds(StopTime),
            if
                StartTimeSecond =< Now andalso Now =< StopTimeSecond ->
                    SelectSql = io_lib:format("select roleID,mark from gTreasureHouse where mark > 0 and activityID = ~w", [ActivityID]),
                    % 插入子服的排名数据
                    MarkList =
                        lists:foreach(fun(PoolID) ->
                                SlavePoolID = get_slave_pool_id(PoolID),
                                {result_packet, _, _, Data,_} = execute(SlavePoolID, SelectSql),
                                lists:foreach(fun([RID,MarkValue])->
                                        {result_packet, _, _, RName,_} = execute(SlavePoolID, io_lib:format("select `roleName` from gRole where roleID = ~w",[RID])),
                                        case RName of
                                            [[RoleName]] ->
                                                activityRank_server:update_treaHouse_roleRankInfo(RID, RoleName, MarkValue);
                                            _ ->
                                                ?LOG("merge_treaHouse name wrong ~w:~w",[RID,RName])
                                        end
                                    end, Data)
                            end, ServerIDList);
                true ->
                    ?LOG("merge_treaHouse time wrong ~w",[ActivityInfo])
            end;    
        ActivityInfo ->
            ?LOG("merge_treaHouse time wrong ~w",[ActivityInfo])
    end,
    %% 保存处理后的结果
    Ets = ets:tab2list(?ETS_TABLE_NAME),
    Info = {TreaHouseState, Ets},
    set_etc(?DB_ETC_KEY_ACTIVITYRANK, Info).

merge_fire(MasterPoolID,ServerIDList)->
    FireRoleInfoList = 
        lists:foldl(fun(PoolID,AccList)-> 
                            SlavePoolID = get_slave_pool_id(PoolID),
                            case get_etc(SlavePoolID, ?DB_ETC_KEY_FIRE_ROLEINFO) of
                                List when erlang:is_list(List) ->
                                    List ++ AccList;
                                _ ->
                                    AccList
                            end
                    end, get_etc(get_master_pool_id(), ?DB_ETC_KEY_FIRE_ROLEINFO), ServerIDList),
    set_etc(?DB_ETC_KEY_FIRE_ROLEINFO, FireRoleInfoList),
    FireRoleState = 
        lists:foldl(fun(PoolID,#d_fire_info{total=AccTotal, rankList=AccRankList, rewardList=AccRewardList} = AccInfo)-> 
                            SlavePoolID = get_slave_pool_id(PoolID),
                            case get_etc(SlavePoolID, ?DB_ETC_KEY_FIRE) of
                                #d_fire_info{total=Total, rankList=RankList, rewardList=RewardList} ->
                                    #d_fire_info{total=max(Total,AccTotal), rankList=RankList ++ AccRankList, rewardList=RewardList++AccRewardList};
                                _ ->
                                    AccInfo
                            end
                    end, get_etc(get_master_pool_id(), ?DB_ETC_KEY_FIRE), ServerIDList),
    %% FireRoleState#d_fire_info.rankList 超过实际长度，没关系，有人祈祷的时候会处理
    NowRankList = lists:sort(fun({_,A,_},{_,B,_})-> A > B end, FireRoleState#d_fire_info.rankList),
    NowRankListLength = erlang:length(NowRankList),
    NowRankList2 = if
                       NowRankListLength > 5 ->
                           {NowRankListHead, _} = lists:split(5, NowRankList),
                           NowRankListHead;
                       true ->
                           NowRankList
                   end,
    set_etc(?DB_ETC_KEY_FIRE, FireRoleState#d_fire_info{rewardList=NowRankList2}).

merge_rebate(MasterPoolID, SlavePoolIDList) ->
    SelectSql = io_lib:format("select value from gETC where `key`=~w;", [?DB_ETC_KEY_REBATE_ROLEINFO]),
    RebateDataList =
    lists:foldr(fun(PoolID, Acc) ->
                {result_packet, _, _, Data,_} = execute(PoolID, SelectSql),
                case Data of
                    [[Bin]] ->
                        Term = binary_to_term(zlib:uncompress(Bin)),
                        case erlang:is_list(Term) of
                            true ->
                                Term ++ Acc;
                            false ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
        end, [], [MasterPoolID|SlavePoolIDList]),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID,
        io_lib:format("replace into gETC values (~w,~s);",[?DB_ETC_KEY_REBATE_ROLEINFO, db_sql:quote(zlib:compress(term_to_binary(RebateDataList)))])).    

init_id(IDName, Key,Table,Base) ->
    ID = init_id2(Key,Table,Base),
    ets:insert(?ETS_ID, {IDName, ID}).

init_id2(Key,Table,Base) ->
    Sql = io_lib:format("select max(~s) from ~s;",[Key,Table]),
    {result_packet, _,_,Rows,_} = execute(get_master_pool_id(), Sql),
    case Rows of
        [[Max]] when is_integer(Max) ->
            next;
        _ ->
            Max=0
    end,
    erlang:max(Max, Base).                                       

gen_robot_data(MasterPoolID) ->
    TermList = gen_config_term_list(),
    tk_config:load_data_name(),
    AllGer = gen_account:all_ger(),
    AllItem = gen_account:all_item(),
    {RobotList, GerList} = 
    lists:foldl(fun(E, Acc) -> gen_account:gen_account(E, Acc, AllGer, AllItem) end, {[],[]}, TermList),
    RobotList2 = lists:map(fun({_, #role{roleID=RoleID,accid=Vaccid,roleName=VroleName,isMale=VisMale,level=Vlevel,exp=Vexp,coin=Vcoin,reputation=Vreputation,
                        gold=Vgold,goldBonus=VgoldBonus,goldUsed=VgoldUsed,unioncoin=VUnioncoin,profoundCrystal=ProfoundCrystal,vipLevel=VvipLevel,goldTotalPaid=VgoldTotalPaid,title=Vtitle,
                        fightPower=VfightPower,lastLogoutTime=VlastLogoutTime,familyID=FamilyID,lastJoinFamily=LastJoinFamily,head=Head,
                        payExtReward=PayExtReward,extRdActTime=ExtRdActTime,location=Location,isFailed=IsFailed,deviceID=Devid,srcType=SrcType,
                        lastLoginTime=LastLoginTime,tasklevel=Tasklevel,teamId=TeamId,plane_level=PlaneLevel,honor=Honor,carloswintime=Carloswintime,
                        carlosequaltime=Carlosequaltime,carloslosetime=Carloslosetime,carlosseasonid=Carlosseasonid,carloslastwintime=Carloslastwintime,
                        carloslastequaltime=Carloslastequaltime,carloslastlosetime=Carloslastlosetime,carloslastseasonid=Carloslastseasonid
                       ,pvppoint=Pvppoint,home_resource=HomeResource,firstPayStatus=FirstPayStatus}}) ->
                [RoleID,Vaccid,VroleName,db_sql:bool2int(VisMale),Vlevel,Vexp,Vcoin,Vreputation,Vgold,VgoldBonus,VgoldUsed,VUnioncoin,ProfoundCrystal,VvipLevel,
                    VgoldTotalPaid,Vtitle,VfightPower,VlastLogoutTime,FamilyID,LastJoinFamily,Head,PayExtReward,ExtRdActTime,Location,IsFailed,Devid,SrcType,
                    LastLoginTime,Tasklevel,PlaneLevel,TeamId,Honor,Pvppoint,Carloswintime,Carlosequaltime,Carloslosetime,Carlosseasonid,Carloslastwintime,Carloslastequaltime,
                    Carloslastlosetime,Carloslastseasonid,HomeResource,FirstPayStatus]
                end, RobotList),
    %% 写gFighterList
    ?LOG("写gFighterList~n", []),
    FighterDataList = lists:map(fun({RoleID, FighterList}) ->
                                    %% <<131,106>> = erlang:term_to_binary([])
                                    [RoleID,erlang:term_to_binary(FighterList), <<131,106>>, 0, 0, <<131,106>>]
                                end, GerList),
    gen_robot_data2(MasterPoolID,FighterDataList),
%%     {InsertSql1, Format1, NewFighterDataList} = db_trans:trans(gFighterList, FighterDataList),
%%     make_sql_batch_by_piece(MasterPoolID, InsertSql1, Format1, NewFighterDataList, ?WRITELIMIT),
    ?LOG("写gGer~n", []),
    %% 写gGer
    GerDataList =
	    lists:foldl(
	        fun({RoleID, RoleGerList}, Acc1) ->
	                lists:foldl(
	                    fun(Ger, Acc2) ->
	                            #ger{gerID=GerID,gerBase=GerBase} = Ger,
	                            #gerBase{gerExp=GerExp
	                                ,gerLevel=GerLevel
	                                ,gerPos=GerPos
	                                ,gerQuality=GerRank
	                                ,gerTypeID=GerTypeID
                                    ,gerBody=GerBody}=GerBase,
	                            [[GerID,RoleID,GerTypeID,GerLevel,GerExp,GerRank,GerPos, <<131,106>>, GerBody]|Acc2]
	                    end, Acc1, RoleGerList)
	        end, [], GerList),
    {InsertSql2, Format2, NewGerDataList} = db_trans:trans(gGer, GerDataList),
    make_sql_batch_by_piece(MasterPoolID, InsertSql2, Format2, NewGerDataList, ?WRITELIMIT),
    ?LOG("生成机器人成功~n", []),
    {ok, RobotList2}.

%% 分段执行机器人插入程序
gen_robot_data2(MasterPoolID,[])->
    ok;
gen_robot_data2(MasterPoolID,RobotList2)->
    {SubList,OtherList} = lists:split(min(200,length(RobotList2)), RobotList2),
    ?ERR("FighterDataList <<<< ~w >>>> ~n", [erlang:length(SubList)]),
    {InsertSql1, Format1, NewFighterDataList} = db_trans:trans(gFighterList, SubList),
    make_sql_batch_by_piece(MasterPoolID, InsertSql1, Format1, NewFighterDataList, ?WRITELIMIT),
    gen_robot_data2(MasterPoolID,OtherList).

gen_config_term_list() ->
    StartRank = do_merge:get(robot_start_rank),
    {ok, TermList} = file:consult(filename:join([tk_config:root_dir(),"config/data_gen_account.config"])),
    lists:foldr(fun({{Rank1, Rank2},{GerStar,GerLevel,EquipStar,EquipLevel}}, Acc) ->
                if
                    StartRank =< Rank1 ->
                        [{{Rank1, Rank2},{GerStar,GerLevel,EquipStar,EquipLevel}}|Acc];
                    StartRank =< Rank2 ->
                        [{{StartRank, Rank2},{GerStar,GerLevel,EquipStar,EquipLevel}}|Acc];
                    true ->
                        Acc
                end
        end, [], TermList).

sync_home_task2(MasterPoolID,[])->
    ?LOG("sync_home_task2 write finsh ~n", []),
    ok;
sync_home_task2(MasterPoolID,TaskList)->
    {SubTaskList,OtherTaskList} = lists:split(min(100,length(TaskList)), TaskList),
    DataSql0 = lists:foldr(fun([_Id,OwnerRoleId, Quality,Level,TGerTypeListStr,TimtoutTs
                    ,Status,AcceptRoleId,GerId,GerType,GerQuality,GerLevel,TaskType
                    ,RewardNum,RoleLevel,BoxRewardBin],AccSql) -> 
                    io_lib:format(",(~w,~w,~w,~s,~w,~w,~w,~s,~s,~s,~s,~w,~w,~w,~s)"
                         ,[OwnerRoleId, Quality,Level,db_sql:quote(TGerTypeListStr),TimtoutTs
                          ,Status,AcceptRoleId,db_sql:quote(GerId),db_sql:quote(GerType),db_sql:quote(GerQuality),db_sql:quote(GerLevel),TaskType
                          ,RewardNum,RoleLevel,db_sql:quote(BoxRewardBin)]) ++ AccSql
        end , " ;", SubTaskList),
    [_|DataSql] = DataSql0,
    CmdSql = "insert into ghometask (owner_role_id,quality,level,tgt_ger_type,timtout_ts,status,accept_role_id,ger_id
                            ,ger_type,ger_quality,ger_level,task_type,reward_num,role_level,box_reward_bin) 
                            values " ++ DataSql, % 去掉第一个逗号
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, CmdSql),
    ?LOG("sync_home_task2 write ~w ~n", [erlang:length(SubTaskList)]),
    sync_home_task2(MasterPoolID,OtherTaskList).    

do_merge_data(ServerIDList) ->
    MasterPoolID = get_master_pool_id(),
    NeedMergeTable = do_merge:get(need_merge_tables),
    AllRoleIDTable = do_merge:get(all_role_id_tables),
    lists:foreach(fun(ServerID) ->
                SlavePoolID = get_slave_pool_id(ServerID),
                lists:foreach(fun(Table) ->
                            CountSql = io_lib:format("select count(1) from ~w", [Table]),
                            {result_packet, _, _, [[Count]], _} = execute(SlavePoolID, CountSql),
                            ?LOG("server:~w,表:~w共有~w条数据.~n", [ServerID, Table, Count]),
                            case Count > ?SELECTLIMIT of
                                true ->
                                    merge_date_by_page(MasterPoolID, ServerID, SlavePoolID, Table, Count, 0);
                                _ ->
                                    SelectSql = io_lib:format("select * from ~w", [Table]),
                                    merge_date_list(MasterPoolID, ServerID, SlavePoolID, Table, SelectSql)
                            end
                    end, NeedMergeTable ++ AllRoleIDTable)
        end, ServerIDList).

do_delete_tables(ServerIDList) ->
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                PoolID =
                case ServerID =:= MasterServerID of
                    true ->
                        get_master_pool_id();
                    false ->
                        get_slave_pool_id(ServerID)
                end,
                ?LOG("~ts,ServerID:~w,PoolID:~w~n", ["开始删除不需要的数据", ServerID, PoolID]),
                lists:foreach(fun(E) -> 
                            Sql = io_lib:format("truncate table ~w", [E]), 
                            {ok_packet,_,_,_,_,_,_} = execute(PoolID,Sql) end, do_merge:get(need_delete_table))
        end, [MasterServerID|ServerIDList]).

%% 清理无效的帐号数据
do_delete_dead_player(ServerIDList, ClearJunkP) ->
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                PoolID =
                case ServerID =:= MasterServerID of
                    true ->
                        get_master_pool_id();
                    false ->
                        get_slave_pool_id(ServerID)
                end,
                do_delete_dead_player2(PoolID, ServerID, ClearJunkP)
        end, [MasterServerID|ServerIDList]),
    ok.

do_delete_dead_player2(PoolID, ServerID, ClearJunkP) ->
    case do_merge:get(is_delete_dead_player) of
        true ->
            NowTimestamp = util:now(),
            OfflineDaysLimit = do_merge:get(offline_days),
            LimitTimestamp = NowTimestamp - OfflineDaysLimit * 24 * 60 * 60,
            LevelLimit = do_merge:get(level),
            PayGoldLimit = do_merge:get(pay_gold),
            SelectSql = io_lib:format("select roleID from gRole where goldTotalPaid <= ~w and level <= ~w and lastLogoutTime <= ~w;", [PayGoldLimit, LevelLimit, LimitTimestamp]),
            {result_packet, _, _, RoleIDList,_} = execute(PoolID, SelectSql);
        false ->
            RoleIDList = []
    end,
    ?LOG("ServerID:~w, 筛选出~w个死号玩家~n", [ServerID, erlang:length(RoleIDList)]),
    RobotIDList = 
        case ClearJunkP of
            false ->
                lists:seq((ServerID + 1) * 1000000 + 1, (ServerID + 1) * 1000000 + 9999);
            _ ->
                []
        end,
    DelRoleIDList = lists:append(RobotIDList, lists:flatten(RoleIDList)),
    AllRoleIDTable = 
        case ClearJunkP of
            false ->
                do_merge:get(all_role_id_tables);
            _ ->
                do_merge:get(junk_role_id_tables) 
        end,
    %% 清理垃圾数据时,需要保留身上的装备和精灵
    case ClearJunkP of
        true ->
            DelGerSql = io_lib:format("delete from `gGer` where `gerPos` = 0 and roleID in", []),
            make_sql_batch_del_by_piece(PoolID, DelGerSql, DelRoleIDList, ?DELETELIMIT),
            DelEquipSql = io_lib:format("delete from `gEquip` where `itemPos` = 0 and roleID in", []),
            make_sql_batch_del_by_piece(PoolID, DelEquipSql, DelRoleIDList, ?DELETELIMIT);
        _ ->
            ignore
    end,
    lists:foreach(
        fun(Table) ->
                ?LOG("ServerID:~w, 删除~w表上的死号数据.~n", [ServerID, Table]),
                DeleteSql = io_lib:format("delete from ~w where roleID in", [Table]),
                make_sql_batch_del_by_piece(PoolID, DeleteSql, DelRoleIDList, ?DELETELIMIT),
                case Table of
                    gFriend ->
                        DeleteSql2 = "delete from gFriend where friendID in",
                        make_sql_batch_del_by_piece(PoolID, DeleteSql2, DelRoleIDList, ?DELETELIMIT);
                    gInviteRoleList ->
                        DeleteSql2 = "delete from gInviteRoleList where inviteRoleID in",
                        make_sql_batch_del_by_piece(PoolID, DeleteSql2, DelRoleIDList, ?DELETELIMIT);
                    _ ->
                        next
                end
        end, AllRoleIDTable).

%% 处理重复
do_process_gpay(SlaveServerIDList) ->
    %% 先跑一次循环，检查哪些gPay主键是重复的
    MasterServerID = data_setting:get(server_id),
    Md5List =
    lists:foldr(
        fun(ServerID, AccMd5List) ->
                PoolID =
                case ServerID =:= MasterServerID of
                    true ->
                        get_master_pool_id();
                    false ->
                        get_slave_pool_id(ServerID)
                end,
                {ok_packet, _,_,_,_,_,_} = execute(PoolID, "alter table gPay change receiptMd5 receiptMd5 varchar(80) not null;"),
                {result_packet, _, _, Rows,_} = execute(PoolID, "select * from gPay"),
                lists:foldr(
                    fun([Md5, _, _, _, _, _], AccMd5List2) ->
                            case lists:keyfind(Md5, 1, AccMd5List2) of
                                false ->
                                    [{Md5, [ServerID]}|AccMd5List2];
                                {Md5, ServerIDList} ->
                                    lists:keyreplace(Md5, 1, AccMd5List2, {Md5, [ServerID|ServerIDList]})
                            end  
                    end, AccMd5List, Rows)
        end, [], [MasterServerID|SlaveServerIDList]),
    DuplicateList =
    lists:foldr(fun({_, ServerIDList} = R, Acc) ->
                case erlang:length(ServerIDList) > 1 of
                    true ->
                        [R | Acc];
                    false ->
                        Acc
                end
        end, [], Md5List),
    ?LOG("有重复情况的gpay一共有~w个~n", [erlang:length(DuplicateList)]),
    ?LOG("~w~n", [DuplicateList]),
    Fix = do_merge:get(rename_fix),
    lists:foreach(
        fun({Md5, ServerIDList}) ->
                lists:foreach(
                    fun(ServerID) ->
                            NewMd5 = erlang:list_to_binary(erlang:binary_to_list(Md5) ++ Fix ++ erlang:integer_to_list(ServerID)),
                            change_gpay_md5(NewMd5, ServerID, Md5)
                    end, ServerIDList)
        end, DuplicateList),
    ok.

%% 修改gpay主键
change_gpay_md5(NewMd5, ServerID, Md5) ->
    MasterServerID = data_setting:get(server_id),
    PoolID =
    case ServerID =:= MasterServerID of
        true ->
            get_master_pool_id();
        false ->
            get_slave_pool_id(ServerID)
    end,
    Sql = io_lib:format("update gPay set receiptMd5= '~s' where receiptMd5 = '~s';",[NewMd5, Md5]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql),
    ok.

do_process_duplicate_name(ServerIDList) ->
    %% 注意:数据库是大小写不敏感的,所以这里需要存一个全大写(或则小写)的名字进行比较, 一个名字用于实际写入数据库
    %% 处理角色重名的情况,并根据规则进行重命名
    do_process_duplicate_name_1(ServerIDList, gRole, "select `roleID`, `roleName` from `gRole`;", 
        fun([RoleID, RoleName]) -> 
                RoleName2 = 
                    case erlang:is_binary(RoleName) of
                        true ->
                            erlang:binary_to_list(RoleName);
                        _ ->
                            RoleName
                    end,
                {RoleID, RoleName, string:to_upper(RoleName2)} end, fun rename_role_name/4),

    %% 处理公会重名的情况,并根据规则进行重命名
    do_process_duplicate_name_1(ServerIDList, gFamily, "select `familyID`, `familyName` from `gFamily`;",
        fun([FamilyID,FamilyName]) -> 
                FamilyName2 = 
                    case erlang:is_binary(FamilyName) of
                        true ->
                            erlang:binary_to_list(FamilyName);
                        _ ->
                            FamilyName
                    end,
                {FamilyID, FamilyName, string:to_upper(FamilyName2)} end, fun rename_family_name/4).

do_process_duplicate_name_1(ServerIDList,TableName,Sql,DealFun,FixFun) ->
    %% 先跑一次循环，检查哪些名字是重复的
    MasterServerID = data_setting:get(server_id),
    NameList =
    lists:foldr(
        fun(ServerID, AccNameList) ->
                PoolID =
                case ServerID =:= MasterServerID of
                    true ->
                        get_master_pool_id();
                    false ->
                        get_slave_pool_id(ServerID)
                end,
                {result_packet, _,_, Rows, _} = execute(PoolID, Sql),
                lists:foldr(
                    fun(Row, AccNameList2) ->
                            {ID, Name, CompareName} = DealFun(Row),
                            NameT = del_tail_space(Name),
                            CompareNameT = del_tail_space(CompareName),
                            case Name =/= NameT of
                                true ->
                                    ?LOG("表:~p出现末尾带空格的名字, Name:~w, NameT:~w, ID:~w, ServerID:~w", [TableName, Name, NameT, ID, ServerID]);
                                false ->
                                    next
                            end,
                            case lists:keyfind(CompareNameT, 1, AccNameList2) of
                                false ->
                                    [{CompareNameT, [{ServerID, ID, NameT}]}|AccNameList2];
                                {CompareNameT, RoleIDList} ->
                                    lists:keyreplace(CompareNameT, 1, AccNameList2, {CompareNameT, [{ServerID, ID, NameT}|RoleIDList]})
                            end
                    end, AccNameList, Rows)
        end, [], [MasterServerID|ServerIDList]),
    DuplicateList =
    lists:foldr(fun({_, IDList} = R, Acc) ->
                    case erlang:length(IDList) > 1 of
                        true ->
                            [R | Acc];
                        false ->
                            Acc
                    end
                end, [], NameList),
    ?LOG("~p:有重复情况的名字一共有~w个~n", [TableName,erlang:length(DuplicateList)]),
    ?LOG("~w~n", [DuplicateList]),
    NameFix = do_merge:get(rename_fix),
    lists:foreach(
        fun({_, IDList}) ->
                lists:foreach(
                    fun({ServerID, ID, OldName}) ->
                            NewName = erlang:list_to_binary(erlang:binary_to_list(OldName) ++ NameFix ++ erlang:integer_to_list(ServerID)),
                            FixFun(ID, NewName, ServerID, OldName)
                    end, IDList)
        end, DuplicateList),
    ok.

del_tail_space(VroleName) when erlang:is_binary(VroleName) ->
    VroleNameList = erlang:binary_to_list(VroleName),
    NewVroleNameList = del_tail_space2(lists:reverse(VroleNameList), false),
    erlang:list_to_binary(NewVroleNameList);

del_tail_space(VroleName) ->
    del_tail_space2(lists:reverse(VroleName), false).

del_tail_space2([], _) ->
    [];
del_tail_space2(List, true) ->
    lists:reverse(List);
del_tail_space2([H|List], false) ->
    case H of
        32 ->
            del_tail_space2(List, false);
        _ ->
            del_tail_space2([H|List], true)
    end.


%% 角色改名，更新gRole，gLimit
rename_role_name(RoleID, NewRoleName, ServerID, OldRoleName) ->
    MasterServerID = data_setting:get(server_id),
    PoolID =
    case ServerID =:= MasterServerID of
        true ->
            get_master_pool_id();
        false ->
            get_slave_pool_id(ServerID)
    end,
    NewRoleName2 = db_sql:quote(NewRoleName),
    GRoleSql = io_lib:format("update gRole set roleName= ~s where roleID = ~w;",[NewRoleName2, RoleID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GRoleSql),

    GLimitSql = io_lib:format("update gLimit set inviteRoleName= ~s where inviteRoleName = ~s;",[NewRoleName2, db_sql:quote(OldRoleName)]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GLimitSql),

    GFamilySql1 = io_lib:format("update gFamily set createRoleName = ~s where createRoleID = ~w;", [NewRoleName2, RoleID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GFamilySql1),

    GFamilySql2 = io_lib:format("update gFamily set ownerRoleName = ~s where ownerRoleID = ~w;", [NewRoleName2, RoleID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GFamilySql2),

    GFamilySql3 = io_lib:format("update gFamilyMember set roleName = ~s where roleID = ~w;", [NewRoleName2, RoleID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, GFamilySql3),

    HomeSteadSql = io_lib:format("update gHomestead set roleName = ~s where roleID = ~w;", [NewRoleName2, RoleID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, HomeSteadSql),
    ok.

%% 公会改名, 更新gFamily
rename_family_name(FamilyID, NewFamilyName, ServerID, _OldRoleName) ->
    MasterServerID = data_setting:get(server_id),
    PoolID =
    case ServerID =:= MasterServerID of
        true ->
            get_master_pool_id( );
        false ->
            get_slave_pool_id(ServerID)
    end,
    Sql = io_lib:format("update `gFamily` set `familyName` = ~s where `familyID` = ~w;", [db_sql:quote(NewFamilyName), FamilyID]),
    {ok_packet, _,_,_,_,_,_} = execute(PoolID, Sql),
    ok.


do_start_log_server() ->
    case merge_log:start_log_server( ) of
        ok ->
            ok;
        _ ->
            erlang:error("日志进程启动失败!")
    end.

do_filter_merge(ServerIDList) ->
    MasterPoolID = get_master_pool_id(),
    MasterServerID = data_setting:get(server_id),
    lists:foreach(fun(ServerID) ->
                DealPoolID = 
                case MasterServerID =:= ServerID of
                    true ->
                        MasterPoolID;
                    _ ->
                        get_slave_pool_id(ServerID)
                end,
                lists:foreach(fun(Table) ->
                            SelectSql = get_filter_merge_sql(Table),
                            ?LOG("ServerID:~w, Table:~w, ~ts~n", [ServerID, Table, SelectSql]),
                            {result_packet, _, _, DataList,_} = execute(DealPoolID, SelectSql),
                            {InsertSql, Format, NewDataList} = db_trans:trans(Table, DataList),
                            ?LOG("ServerID:~w, Table:~w, ~ts~n", [ServerID, Table, InsertSql]),
                            case MasterServerID =:= ServerID of
                                true ->
                                    %%如果是主服则先把这个表清空,再重新插入
                                    TruncSql = io_lib:format("truncate table ~w", [Table]),
                                    {ok_packet,_,_,_,_,_,_} = execute(DealPoolID, TruncSql);
                                _ ->
                                    ignore
                            end,
                            make_sql_batch_by_piece(MasterPoolID, InsertSql, Format, NewDataList, ?WRITELIMIT)
                    end, do_merge:get(filter_merge_tables))
        end, [MasterServerID|ServerIDList]).

merge_date_by_page(MasterPoolID, ServerID, SlavePoolID, Table, Total, Start) ->
    SelectSql = io_lib:format("select * from ~w limit ~w, ~w", [Table, Start, ?SELECTLIMIT]),
    merge_date_list(MasterPoolID, ServerID, SlavePoolID, Table, SelectSql),
    NewStart = Start + ?SELECTLIMIT,
    case NewStart >= Total of
        true ->
            ignore;
        _ ->
            merge_date_by_page(MasterPoolID, ServerID, SlavePoolID, Table, Total, NewStart)
    end.

merge_date_list(MasterPoolID, ServerID, SlavePoolID, Table, SelectSql) ->
    ?LOG("ServerID:~w, Table:~w, ~ts~n", [ServerID, Table, SelectSql]),
    {result_packet, _, _, DataList,_} = execute(SlavePoolID, SelectSql),
    {InsertSql, Format, NewDataList} = db_trans:trans(Table, DataList),
    ?LOG("ServerID:~w, Table:~w, ~ts~n", [ServerID, Table, InsertSql]),
    make_sql_batch_by_piece(MasterPoolID, InsertSql, Format, NewDataList, ?WRITELIMIT).

get_filter_merge_sql(gMail) ->
    DelTimeMask = util:now() - do_merge:get(mail_overdue_day) * 86400,
    io_lib:format("SELECT * FROM `gMail` WHERE `mailType` IN (~w, ~w) AND `time` > ~w;", [?MAIL_TYPE_SYS, ?MAIL_TYPE_REWARD, DelTimeMask]);

get_filter_merge_sql(Table) -> 
    Error = io_lib:format("~w unknown filter tables", [Table]),
    error(Error).

do_process_etc(ServerIDList) ->
%%     do_process_get_energy(ServerIDList),
    do_process_team_pk(ServerIDList),
    do_process_carlos(ServerIDList),
    do_process_relic(ServerIDList),
    do_process_galactica(ServerIDList),
	do_process_twins(ServerIDList).

%% do_process_get_energy(ServerIDList)->
%%     AllList = lists:foldl(fun(ServID,AccList)-> 
%%                         SlavePoolID = get_slave_pool_id(ServID),
%%                         get_etc(SlavePoolID, ?DB_ETC_KEY_ENERGY_ROLELIST) ++ AccList 
%%                 end, get_etc(get_master_pool_id(), ?DB_ETC_KEY_ENERGY_ROLELIST), ServerIDList),
%%     set_etc(?DB_ETC_KEY_ENERGY_ROLELIST, AllList).
  
do_process_team_pk(ServerIDList) ->
    ?LOG("do_process_team_pk~n", []),
    MasterPoolID = get_master_pool_id(),
    [{state, MasterState}|AllInfo] = get_etc(MasterPoolID, ?DB_ETC_KEY_TEAM_PK),
%%     case get_etc(MasterPoolID, ?DB_ETC_KEY_TEAM_PK) of
%%         [{state,State0}|AllInfo0] ->
%%             AllInfo = AllInfo0,
%%             MasterState = State0;
%%         _ ->
%%             % 都不会是新服，通常不会走这个分支
%%             AllInfo = [],
%%             MasterState = {state,0,0,0,0,0,[],[],[]}
%%     end,
    MasterRoleIDList = erlang:element(7, MasterState),
    MasterRankList = erlang:element(8, MasterState),
    {FinalyRoleIDList, FinalyRankList} = 
        lists:foldl(fun(ServerID, {IDAcc, RankAcc}) ->
                        SlavePoolID = get_slave_pool_id(ServerID),
                        [{state, State}|_] = get_etc(SlavePoolID, ?DB_ETC_KEY_TEAM_PK),
                        RoleIDList = erlang:element(7, State),
                        RankList = erlang:element(8, State),
                        {IDAcc ++ RoleIDList, RankList ++ RankAcc}
                    end, {MasterRoleIDList, MasterRankList}, ServerIDList), 
    {FinalyRankList2, _, _} = team_pk_server:sort_and_return(0, FinalyRankList),
    FinalyState1 = erlang:setelement(7, MasterState, FinalyRoleIDList),
    FinalyState2 = erlang:setelement(8, FinalyState1, FinalyRankList2),
    set_etc(?DB_ETC_KEY_TEAM_PK, [{state, FinalyState2}|AllInfo]).

%% 处理卡洛斯的购买次数
do_process_carlos(ServerIDList) ->
    ?LOG("do_process_carlos~n", []),
    MasterPoolID = get_master_pool_id(),
    [MState, {MCarlosInfo, MCarlosSignInfo, MCarlosRoleTimes}] = get_etc(MasterPoolID, ?DB_ETC_KEY_CARLOS),
    TCarlosRoleTimes = 
        lists:foldl(fun(E, Acc) ->
                        SlavePoolID = get_slave_pool_id(E),
                        [_, {_, _, SCarlosRoleTimes}] = get_etc(SlavePoolID, ?DB_ETC_KEY_CARLOS),
                        SCarlosRoleTimes ++ Acc
                    end, MCarlosRoleTimes, ServerIDList),
    set_etc(?DB_ETC_KEY_CARLOS, [MState, {MCarlosInfo, MCarlosSignInfo, TCarlosRoleTimes}]).

%% 处理巨龙
do_process_relic(ServerIDList) ->
    ?LOG("do_process_relic~n", []),
    MasterPoolID = get_master_pool_id(),
    {Date, MRInfoList} = get_etc(MasterPoolID, ?DB_ETC_KEY_RELIC_ROLE_INFO),
    TRInfoList = 
        lists:foldl(fun(E, Acc) ->
                        SlavePoolID = get_slave_pool_id(E),
                        {_, SRInfoList} = get_etc(SlavePoolID, ?DB_ETC_KEY_RELIC_ROLE_INFO),
                        SRInfoList ++ Acc
                    end, MRInfoList, ServerIDList),
    set_etc(?DB_ETC_KEY_RELIC_ROLE_INFO, {Date, TRInfoList}).

%% galactica
do_process_galactica(ServerIDList) ->
    ?LOG("do_process_galactica~n", []),
    MasterPoolID = get_master_pool_id(),
    [{RW, MRTimes}] = get_etc(MasterPoolID, ?DB_ETC_KEY_GALACTICA),
    TRTimes = 
        lists:foldl(fun(E, Acc) ->
                        SlavePoolID = get_slave_pool_id(E),
                        [{_, SRTimes}] = get_etc(SlavePoolID, ?DB_ETC_KEY_GALACTICA),
                        SRTimes ++ Acc
                    end, MRTimes, ServerIDList),
    set_etc(?DB_ETC_KEY_GALACTICA, [{RW, TRTimes}]).

%% twins
do_process_twins(ServerIDList) ->
    ?LOG("do_process_galactica~n", []),
    MasterPoolID = get_master_pool_id(),
    [{RW, MRTimes}] = get_etc(MasterPoolID, ?DB_ETC_KEY_TWINS),
    TRTimes = 
        lists:foldl(fun(E, Acc) ->
                        SlavePoolID = get_slave_pool_id(E),
                        [{_, SRTimes}] = get_etc(SlavePoolID, ?DB_ETC_KEY_TWINS),
                        SRTimes ++ Acc
                    end, MRTimes, ServerIDList),
    set_etc(?DB_ETC_KEY_TWINS, [{RW, TRTimes}]).
    
get_etc(PoolID, Key) ->
	Sql = io_lib:format("select value from gETC where `key`=~w;", [Key]),
    {result_packet, _, _, DataList,_} = execute(PoolID, Sql),
	case DataList of
		[Bin] ->
			db_sql:uncompress_decode(Bin,[]);
		_ ->
			[]
	end.

set_etc(Key, Info) ->
    MasterPoolID = get_master_pool_id(),
	Bin = db_sql:compress_encode(Info),
	Sql = io_lib:format("replace into gETC values (~w,~s);", [Key, db_sql:quote(Bin)]),
    {ok_packet, _,_,_,_,_,_} = execute(MasterPoolID, Sql).
