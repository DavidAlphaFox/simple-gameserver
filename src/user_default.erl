%% @author caohongyang
%% @doc 常用命令模块
%% Created 2013-2-20


-module(user_default).
-include("def_role.hrl").
-include("def_battle.hrl").
-include_lib("kernel/include/file.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

num() ->
	ets:info(?ETS_ROLE_ONLINE,size).

test() ->
	?CATCH(erlang:error(certain_error)).

app(List) ->
	tk_misc:start_applications(List).

log(Term) ->
	?ERR("Log: ~1000000p",[Term]).

emu(RoleID, Msg) ->
	erlang:send(role_lib:gatewayRegName(RoleID),{emu, Msg}).

a(Tab) ->
	ets:tab2list(Tab).

a() ->
	RoleIDList = [E||{E,_}<-a(?ETS_ROLE_ONLINE)],
	First100 = lists:sublist(RoleIDList, 100),
	[begin
		 case catch ri(RoleID) of 
			 #role{roleName=Name} ->
				 show(Name),
				 io:format("\t\t~w\n",[RoleID]);
			 _ ->
				 io:format("error\t\t~w\n",[RoleID])
		 end
	 end || RoleID <- First100],
	io:format("\n").

refresh_onliners() ->
	RoleIDList = [E||{E,_}<-a(?ETS_ROLE_ONLINE)],
	[begin
		 case catch ri(RoleID) of 
			 #role{roleName=Name} ->
				 show(Name),
				 io:format("\t\t~w\n",[RoleID]);
			 _ ->
				 catch(erlang:unregister(role_lib:regName(RoleID))),
				 role_lib:leave_online_table(RoleID),
				 ets:delete(role_state,RoleID)
		 end
	 end || RoleID <- RoleIDList],
	RoleIDList2 = [E||{E,_}<-a(role_state)],
	[begin
		 case catch ri(RoleID) of 
			 #role{roleName=Name} ->
				 show(Name),
				 io:format("\t\t~w\n",[RoleID]);
			 _ ->
				 catch(erlang:unregister(role_lib:regName(RoleID))),
				 role_lib:leave_online_table(RoleID),
				 ets:delete(role_state,RoleID)
		 end
	 end || RoleID <- RoleIDList2],
	io:format("\n").


id() ->
	case ets:first(?ETS_ROLE_ONLINE) of
		'$end_of_table' ->
			0;
		ID->
			ID
	end.

rid() ->
    OnLineList = ets:tab2list(?ETS_ROLE_ONLINE), 
    util:random_one_from_list(OnLineList).

s() ->
	tk:start().

stop() ->
	tk:stop().

t() ->
	role_battle:test().

l() ->
	tclient:login("dsfredsf").

%% 重新加载全部配置
lc()->
	tk_config:preload_config().
lc(A) ->
	tk_config:reload_config(A).
lc(A,B,C,D) ->
	tk_config:reload_config(A,B,C,D).
	
%% 重新生成协议
pg() ->
	RootDir = tk_config:root_dir(),
	proto_compile:scan_dir(filename:join([RootDir, "proto"]),filename:join([RootDir, "src"])).

start_plunder_tick()->
    erlang:send_after(1000, plunder_server, check_recover_tick).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 清除所有历史日志
cl() ->
	Dir = data_setting:get(logger_file_dir),
	filelib:fold_files(Dir, ".+", false, fun(E,_) -> io:format("~s\n",[E]),ok = file:delete(E) end, ok).

p(Name) when is_atom(Name) ->
	whereis(Name);
p(RoleID) when is_integer(RoleID) ->
	whereis(role_lib:regName(RoleID));
p(Pid) when is_pid(Pid) ->
	Pid.
	

d(P) ->
	element(2, process_info(p(P),dictionary)).
	
d(P,Key) ->
	Dict = element(2, process_info(p(P),dictionary)),
	case lists:keyfind(Key, 1, Dict) of
		false ->
			?undefined;
		{_, Value} ->
			Value
	end.



put(P, Key, Value) ->
	erlang:send(p(P), {func, fun erlang:put/2, [Key, Value]}).

i(P) ->
	gen_server:call(p(P), i).

ri(P) ->
	d(P, roleInfo).

kill(P) ->
	exit(p(P), kill).

%% 做xref分析
xref() ->
	List = xref:d(filename:join([tk_config:root_dir(), "ebin"])),
	{value, {undefined, UndefList}, List2} = lists:keytake(undefined, 1, List),
	UndefList2 = 
	lists:filter(fun(E) ->
						 EL = tuple_to_list(E),
						 Last = lists:last(EL),
						 if element(1, Last) == logger ->
								false;
							true ->
								true
						 end							 
				 end, UndefList),
	List2 ++ [{undefined,UndefList2}].
	

reload() ->
	LibDir = lib_dir(),
	%% 蛋疼的代码，在windows下，盘符，有时返回E:,有时返回e:
	[erlang:insert_element(2, c:l(E), E)||
	   {E,P}<-code:all_loaded(),P=/=preloaded, 
	   {P2,_} <-[filename:find_src(E)],
	   not(is_list(P2) andalso lists:prefix(LibDir, filename:dirname(P2)))].

lib_dir() ->
	{F, _Options} = filename:find_src(lists),
	filename:dirname(filename:dirname(filename:dirname(F))).

r(Mod) ->
	tk_misc:compile(Mod, tk_config:root_dir()),
	c:l(Mod).

show(Str0) ->
	Str = if is_binary(Str0) -> binary_to_list(Str0); true ->Str0 end,
	io:format("~ts",[unicode:characters_to_binary(Str,utf8,latin1)]).



stack(P) ->
	process_info(p(P), current_stacktrace).

w(Term) ->
	file:write_file("temp.txt",io_lib:format("~p",[Term]),[write]).

gc() ->
	lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()).

z() ->
	make:all(),
	reload(),
	ok.

func(P, F, Args) ->
	gen_server:call(p(P), {func, F, Args}).

n() ->
	ets:info(?ETS_ROLE_ONLINE, size).



%% 编译并热更新某个模块
le(M) ->
	File = find_src(M),
	{ok,[{_F, Options}|_]} = file:consult(filename:join([tk_config:root_dir(),"Emakefile"])),
	compile:file(File, Options),
	c:l(M).

find_src(M) ->
	FileName = atom_to_list(M)++".erl",
	Dir = tk_config:root_dir(),
	filelib:fold_files(Dir, FileName, true, fun(E,_) ->
														  io:format("~s",[E]),
														  E end, 0).

rw(A) ->
	io:format("~w",[A]).

%%  测试命令
tbc(N) ->
	util:for(1, N, 
			 fun(_) ->
					 broadcast_server:bc_msg("a时天使天使时天使天使时天使天使时天使天使天时天使天使！！~n") 
			 end).

tbc(Msg0, N) ->
	Msg2 = unicode:characters_to_binary(Msg0),
	util:for(1, N,
		fun(_) ->
			broadcast_server:bc_msg(Msg2)
		end).

check_normal(RoleID) ->
    case process_info(role_lib:pid(RoleID),current_function) of
        {current_function,{gen_server,loop,6}} -> {normal,ets:lookup(role_state,RoleID),ets:lookup(ets_role_online,RoleID)};
        X -> X
    end.

kill_role(RoleID) ->
    catch kill(role_lib:pid(RoleID)),
    ets:delete(role_state, RoleID),
    ets:delete(ets_role_online,RoleID).

ss(State) ->
	state_sync_server:sync_state(State).

purchase(RoleID,Num) ->
	List = [data_pay:get(E)||E<-data_pay:get_list()],
	case lists:keyfind(Num, #data_pay.payGold, List) of
		false ->
			?ERR("purchase bad arg:~w",[Num]);
		#data_pay{payID=PayID} ->
			Receipt = integer_to_list(RoleID)++integer_to_list(PayID)++integer_to_list(util:now()),
			Md5 = md5(Receipt),
			gen_server:call(pay_server, {func, fun pay_server:do_pay/5, [RoleID, PayID, Receipt, Md5,0]})
	end.

change_role_vip(RoleID, VipLevel) when VipLevel =< 16 ->
	?ERR("role vip changed:~w,~w",[RoleID,VipLevel]),
	case role_lib:is_online(RoleID) of
		true ->
			erlang:send(role_lib:pid(RoleID),{change_role_vip_level, VipLevel,ensure_action});
		_ ->
			Sql=io_lib:format("update gRole set vipLevel=~w where roleID = ~w;",[VipLevel,RoleID]),
			db_sql:sql_execute_with_log(Sql)
	end.

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

gr() ->
    get_role_process_list().

get_role_process_list() ->
    [X||X<-erlang:registered(),
        "role" == lists:sublist(erlang:atom_to_list(X), 4),
        erlang:is_integer(catch erlang:list_to_integer(erlang:atom_to_list(X) -- "role"))].

gf() ->
    get_family_process_list().

get_family_process_list() ->
    [X||X<-erlang:registered(),
        "family_" == lists:sublist(erlang:atom_to_list(X), 7),
        erlang:is_integer(catch erlang:list_to_integer(erlang:atom_to_list(X) -- "family_"))].

load_all_beam() ->
    {ok, BeamFileList} = file:list_dir("./ebin"),
    lists:foreach(fun(BeamFile) ->
                          BeamFile2 = lists:reverse(BeamFile),
                          case BeamFile2 of
                              "maeb." ++ BeamMod ->
                                  Mod = erlang:list_to_atom(lists:reverse(BeamMod)),
                                  io:format("~w~n", [Mod]),
                                  c:l(Mod);
                              _ ->
                                  io:format("File:~ts~n", [BeamFile2]),
                                  false
                          end
                  end, BeamFileList).

create_table() ->
    ets:new(?ETS_ROLE_DUMP, [{keypos, 1}, set, public, named_table]),
    ets:new(?ETS_STOP_FLAG, [{keypos, 1}, set, public, named_table]).


%% 获得进程内容
get_process_info(Server) ->
    case erlang:whereis(Server) of
        ?undefined ->
            [];
        Pid ->
            erlang:process_info(Pid)
    end.
            
%% 获得进程字典
get_process_dictionary(Server) ->
    case get_process_info(Server) of
        [] ->
            [];
        Info ->
            proplists:get_value(dictionary, Info)
    end.


save_stonechip()->
    %遍历数据库表中存在，但ets表中不存在的符文碎片，予以删除
    CacheList1 = ets:match_object(ets_stonechip_list,'$1'),
    CacheList2 = [{RoleID,StoneType}||{{RoleID,StoneType},_,_,_,_,_} <- CacheList1],    
    lists:foreach(fun([RoleId_DB,StoneType_DB,_,_,_,_])->
                        case lists:member({RoleId_DB,StoneType_DB}, CacheList2) of
                            false ->%确定该数据需要删除
                                ?INFO("need delete ~w ~n",[{RoleId_DB,StoneType_DB}]),
                                db_sql:del_stonechip(RoleId_DB,StoneType_DB);
                            true ->
                                ignore
                        end
            end, db_sql:get_stonechip_all()),
    %重新刷新数据至数据库，为beam文件更新做好准备。
    plunder_server:persist_stonechip_info().

%%修改公会科技等级
test_familyTek_level(FamilyID,TekID,Level)->
	OldTekInfo = ets:lookup(?ETS_FAMILY_TECHNOLOGY, FamilyID),
	case OldTekInfo of
		[] ->
			?ERR("can not find OldTekInfo in ETS_FAMILY_TECHNOLOGY ~n");
		[{_,TekList}] ->
			TekWithOutLevelList = [{TekID2 div 1000,TekLevel}||{TekID2,TekLevel}<-TekList],
			case lists:keyfind(TekID,1,TekWithOutLevelList) of
				false ->
					?ERR("can not find TekInfo with ~w ~n",[TekID]);
				{TekIDWithOutLevel,OldLevel}->
					NewTekID = TekIDWithOutLevel*1000+OldLevel,
					family_misc:router_to_family_process(FamilyID,{fix_familyTek_level,FamilyID,NewTekID,Level})
			end
	end.

kill_zombie() ->
    kill_zombie(50).

kill_zombie(Max) ->
	RoleIDList = [E||{E,_}<-a(?ETS_ROLE_ONLINE)],
    lists:foreach(fun(RoleID) ->
                    Pid = p(RoleID),
                    case Pid =:= undefined of 
                        true ->
				            ets:delete(role_state,RoleID);
                        _ ->
                            PInfo = erlang:process_info(Pid),
                            CurrentF = proplists:get_value(current_function, PInfo),
                            Len = proplists:get_value(message_queue_len, PInfo, 0),
                            case CurrentF =:= {prim_inet,send,3} andalso Len >= Max of
                                true ->
                                    kill(Pid),
				                    ets:delete(role_state,RoleID);
                                _ ->
                                    ignore
                            end
                    end
                end, RoleIDList).


do_clean_unioncoin(_)->
	F = fun()->
				Role = role_data:get_roleInfo(),
				case Role#role.unioncoin > 20010 of
					true ->
						Need = Role#role.unioncoin-20000,
						role_lib:deduct_unioncoin_f(Role,Need,1,1,"");
					_ ->
						ignore
				end
		end,
	role_lib:send_every_server_msg({func,F,[]}).
				

extract_family_log(_) ->
	IDs = db_sql:get_all_family_id(),
	[family_misc:router_to_family_process(ID,extract_log)||ID<-IDs].

reset_family_level(StartSec,StopSec,Lv) ->
	IDs= db_sql:get_all_family_id(),
	[family_misc:router_to_family_process(ID,{reset_family_level, StartSec,StopSec,Lv})||ID<-IDs].

reset_family_level3(StartSec,StopSec,Level, Log) ->
	{NewLv,IsChanged} = 
		lists:foldl(fun(#p_family_log_dtl{timeStamp=TS,type=Type,arg_1=ArgID},{Lv,IsChanged}) ->
							case Type of
								1011 ->
									if TS > StartSec andalso TS < StopSec ->
										   Lv0 = list_to_integer(ArgID),
										   if  Lv0 < Lv ->
												  {Lv0,1};
											  true ->
												  {Lv,IsChanged}
										   end;
									   true ->
										   {Lv,IsChanged}
									end;
								_ ->
									{Lv,IsChanged}
							end
					end, {Level,0}, Log),
	{NewLv,IsChanged}.
re_get_family_sfind(StartSec,StopSec,FileName,1) ->
	{ok, Fd} = file:open(FileName, [raw, binary]),
	re_get_family_sfind(StartSec,StopSec,Fd).

re_get_family_sfind(StartSec,StopSec,Fd)->
	case file:read_line(Fd) of
		eof ->
			ok;
		{ok,Line} ->
			case re:run(Line,".*record_family_log:(\\d*),(\\d),(.*)",[{capture, [1,2,3],list}]) of
				nomatch ->
					re_get_family_sfind(StartSec,StopSec,Fd);
				{match, [FamilyID,FamilyLevel,MFLog]} ->
					MFLog1 = parse_str(MFLog),
					{NewLv,IsChanged} = reset_family_level3(StartSec,StopSec
											   ,list_to_integer(FamilyLevel)
											   ,MFLog1),
					?ERR("info:~s,~w,~w",[FamilyID,NewLv,IsChanged]),
					case IsChanged of
						0 ->
							ignore;
						1 ->
							FamilyID1 = list_to_integer(FamilyID),
							?CATCH(family_misc:router_to_family_process(FamilyID1,{reset_family_level,FamilyID1,NewLv}))
					end,
					re_get_family_sfind(StartSec,StopSec,Fd)
			end
	end.

parse_str(Str) ->
	{_,Tokens,_} = erl_scan:string(Str++"."),
	{_,Term} = erl_parse:parse_term(Tokens),
	Term.

test(FileName)->
	case file:open(FileName,[raw,binary]) of
		{ok,Fd}->
			find(Fd);
		_ ->
			io:format("open file:~w failed ~n",[FileName])
	end.
% {{guessed_role_id,10027491},{9,2,8011373}},
find(Fd)->
	case file:read_line(Fd) of
		eof->
			file:close(Fd),
			ok;
		{ok,Line}->
			case re:run(Line,"{{guessed_role_id,(\\d*)},{(\\d*),(\\d*),(\\d*)}},",[{capture,[1],list}]) of
				nomatch ->
					find(Fd);
				{match,Find}->
					?ERR("Find:~w ~n",[Find]),
				% {match,[RoleID,ServerID,Type,TarRoleID]}->
					% Coin = get_coin_by_type(Type),
					% IoList = io_lib:format("玩家ID：~s 区服：~s 徽章：~w 竞猜的目标玩家：~s\n", [RoleID,ServerID,Coin,TarRoleID]),
					% file:write_file("out.txt", IoList, [raw, append, delayed_write]),
					find(Fd)
			end
	end.
        % {1, 2000},
        % {2, 4000},
        % {3, 8000}
get_coin_by_type(Type)->
	case Type of
		"1"->
			2000;
		"2"->
			4000;
		"3"->
			8000;
		_ ->
			?ERR("出现未知的竞猜类型：~w ~n",[Type]),
			0
	end.

test_set_talent(RoleID)->
	IDList = data_talent:get_list(),
	Msg = lists:foldr(fun(ID,Acc)->
			#data_talent{max_level=MaxLevel,talent_type=Type} = data_talent:get(ID),
			case lists:keyfind(Type,1,Acc) of
				false->
					[{Type,MaxLevel,0}|Acc];
				_->
					Acc
			end
	end,[],IDList),
	?ERR("Msg:~w ~n",[Msg]),
	case catch role_lib:send_server(RoleID, {test_set_talent,Msg}) of
                {'EXIT',_}->
                	?ERR("Exit");
                _ ->
                    ignore                            
    end.

-define(hardlimit, 10001).
-define(normallimit, 30001).
fixup_dungen( ) ->
    fixup_dungen({{2016,4,25},{11,36,50}}, {{2016,4,25},{13,10,0}}).

fixup_dungen(StartTime, EndTime) ->
    spawn(fun() -> fixup_dungen_1(StartTime, EndTime) end).

fixup_dungen_1(StartTime, EndTime) ->
	%select `roleID`, `dungenID` from logdungenfight_2016_4 where datetime > '2016-4-25 11:36:00' and datetime < '2016-4-25 13:10:00' and roleID = 57028795;;
    Sql = io_lib:format("select `roleID`, `dungenID` from logdungenfight_2016_4 where datetime > '~s' and datetime < '~s' and dungenID > 10000 and result=1 and type = 1;", [db_sql:datetime(StartTime), db_sql:datetime(EndTime)]),
    case db_sql:get_all(Sql) of
        [] ->
            ?ERR("fixup_dungen done.no change.~n", []);
        DbList ->
            ToDealList = 
                lists:foldl(fun([RoleID, DungenID], Acc) ->
                                case lists:keytake(RoleID, 1, Acc) of
                                    false ->
                                        [{RoleID, [DungenID]} | Acc];
                                    {value, {_, NowList}, Rest} ->
                                        [{RoleID, [DungenID|NowList]}|Rest]
                                end
                            end, [], DbList),
            
            lists:foreach(fun({RoleID, DungeonList}) ->
                                case role_lib:is_online(RoleID) of
                                    false ->
                                        fixup_offline_role_dungeon(RoleID, DungeonList);
                                    _ ->
                                        role_lib:send_server(RoleID, {fixup_online_role_dungeon, RoleID, DungeonList})
                                end
                        end, ToDealList),
            ?ERR("fixup_dungen done.~n", []) 
    end.

fixup_offline_role_dungeon(RoleID, DungeonList) ->
    {NormalList, HardList} = lists:partition(fun(E) -> E >= ?normallimit end, DungeonList),
    fixup_offline_role_dungeon_1(RoleID, NormalList, "battleProgress", ?normallimit), 
    fixup_offline_role_dungeon_1(RoleID, HardList, "battleProgressHard", ?hardlimit). 

fixup_offline_role_dungeon_1(_, [], _, _) ->
    ignore;

fixup_offline_role_dungeon_1(RoleID, DungeonList, DBKey, Limit) ->
    MaxID = lists:max(DungeonList),
    [DBMaxID] = get_now_process(RoleID, DBKey),
    case MaxID < DBMaxID - 1 of
        true ->
            ?ERR("fixup_offline_role_dungeon,type:~p,RoleID:~p,DungeonList_MaxID:~p,DBMaxID:~p~n", [Limit,RoleID, MaxID, DBMaxID]); 
        _ ->
            ChapterList = get_chapterlist_from_dungeonlist(DungeonList),
        
            ChapterList2 = 
                lists:foldl(fun({ChapterID, SubDungenList}, Acc) ->
                                case db_sql:get_chapter(RoleID, ChapterID) of
                                    ?undefined ->
                                        Acc;
                                    #chapter{dungeonList=CDungenList} = Chapter ->
                                        CDungenList2 = lists:filter(fun(#p_dungeon{dungeonID=E}) -> not lists:member(E, SubDungenList) end, CDungenList),
                                        Chapter2 = Chapter#chapter{dungeonList=CDungenList2},
                                        [Chapter2|Acc]
                                end
                            end, {[], []}, ChapterList),
            db_sql:set_chapter(RoleID, ChapterList2),
            fixup_del_dungeonlist(RoleID, DungeonList),
            %fixup_del_chapterlist(RoleID, DelChapterList),
            MinValue = get_type_min(DungeonList, Limit),
            fixup_reset_process(RoleID, DBKey, MinValue),
            ?ERR(">>>fixup_offline_role_dungeon,type:~p,RoleID:~p,DungeonList:~p~n", [Limit,RoleID, DungeonList]) 
    end.

fixup_online_role_dungeon(RoleID, DungeonList) ->
    {NormalList, HardList} = lists:partition(fun(E) -> E >= ?normallimit end, DungeonList),
    fixup_online_role_dungeon_1(RoleID, NormalList, "battleProgress", ?normallimit),
    fixup_online_role_dungeon_1(RoleID, HardList, "battleProgressHard", ?hardlimit). 

fixup_online_role_dungeon_1(_, [], _, _) ->
    ignore;

fixup_online_role_dungeon_1(RoleID, DungeonList, DBKey, Limit) ->
    MaxID = lists:max(DungeonList),
    [DBMaxID] = get_now_process(RoleID, DBKey),
    case MaxID < DBMaxID - 1 of
        true ->
            ?ERR("fixup_online_role_dungeon,type:~p,RoleID:~p,DungeonList_MaxID:~p,DBMaxID:~p~n", [Limit,RoleID, MaxID, DBMaxID]); 
        _ ->
            ChapterList = get_chapterlist_from_dungeonlist(DungeonList),
        
            lists:foreach(fun({ChapterID, SubDungenList}) ->
                            case role_battle:get_chapter(RoleID, ChapterID) of
                                ?undefined ->
                                    ignore;
                                #chapter{dungeonList=CDungenList} = Chapter ->
                                    CDungenList2 = lists:filter(fun(#p_dungeon{dungeonID=E}) -> not lists:member(E, SubDungenList) end, CDungenList),
                                    Chapter2 = Chapter#chapter{dungeonList=CDungenList2},
                                    role_battle:set_chapter(Chapter2) 
                            end
                        end, ChapterList),
            fixup_del_dungeonlist(RoleID, DungeonList),
            %fixup_del_chapterlist(RoleID, DelChapterList),
            MinValue = get_type_min(DungeonList, Limit),
            if
                Limit =:= ?normallimit ->
        	        role_battle:set_progress(?BATTLE_DUNGEON_TYPE_NORMAL,MinValue);
                Limit =:= ?hardlimit ->
        	        role_battle:set_progress(?BATTLE_DUNGEON_TYPE_HARD,MinValue);
                true ->
                    ignore
            end,
            ?ERR(">>>fixup_online_role_dungeon,type:~p,RoleID:~p,DungeonList:~p~n", [Limit,RoleID, DungeonList]) 
    end.

get_chapterlist_from_dungeonlist(DungeonList) ->
    lists:foldl(fun(DungenID, Acc) ->
                    #data_dungeon{chapterID=ChapterID} = data_dungeon:get(DungenID),
                        case lists:keytake(ChapterID, 1, Acc) of
                            false ->
                                [{ChapterID, [DungenID]}|Acc];
                            {value, {_, NowList}, Rest} ->
                                [{ChapterID, [DungenID|NowList]}|Rest]
                        end
                    end, [], DungeonList).
                        
fixup_del_dungeonlist(RoleID, []) ->
    ignore;

fixup_del_dungeonlist(RoleID, DungeonList) ->
    Arg = string:join([integer_to_list(E) || E <- DungeonList], ","),
    Sql = io_lib:format("delete from gDungeon where `roleID` = ~w and  `dungeonID` in (~s);", [RoleID, Arg]),
    db_sql:sql_execute_with_log(Sql).
                
fixup_del_chapterlist(RoleID, []) ->
    ignore;

fixup_del_chapterlist(RoleID, DelChapterList) ->
    Arg = string:join([integer_to_list(E) || E <- DelChapterList], ","),
    Sql = io_lib:format("delete from gChapter where `roleID` = ~w and `chapterID` in (~s);", [RoleID, Arg]),
    db_sql:sql_execute_with_log(Sql),

    Sql2 = io_lib:format("delete from gBestPassChapter where `roleID` = ~w and `chapterID` in (~s);", [RoleID, Arg]),
    db_sql:sql_execute_with_log(Sql2).

get_type_min(TypeList, Limit) ->
    Min = lists:min(TypeList),
    Min2 = Min - 1,
    case Min2 < Limit of
        true ->
            Limit;
        _ ->
            Min2
    end.

get_type_max([]) ->
    false;

get_type_max(TypeList) ->
    lists:max(TypeList).

fixup_reset_process(RoleID, DBKey, MinValue) ->
    Sql = io_lib:format("update gRoleExtra set ~s = ~w where `roleID` = ~w ;", [DBKey, MinValue, RoleID]),
    db_sql:sql_execute_with_log(Sql).

get_now_process(RoleID, Type) ->
    Sql = io_lib:format("select ~s from gRoleExtra where `roleID` = ~w ;", [Type, RoleID]),
    db_sql:get_row(Sql).

clear_plane_ai()->
    Sql = io_lib:format("UPDATE `groleextra` SET `plane_ai_flag`='0' WHERE `roleID`>'0';", []),
    db_sql:get_row(Sql).

%%此接口直接对数据库进行操作，故不能有玩家在线，否则会有错误    
transform_gold2goldbonus()->
	Sql = io_lib:format("select roleID,gold,goldbonus,vipLevel,srcType from gRole where gold>0;",[]),
	{ok,List} = db_sql:sql_execute_with_log(Sql),
	lists:foreach(fun(RoleID)->
		transform_gold2goldbonus2(RoleID)
	end,List).

transform_gold2goldbonus2([RoleID,OldGold,OldGoldBonus,VipLevel,SrcType])->
	NewGoldBonus = OldGold+OldGoldBonus,
	Sql = io_lib:format("update gRole set gold=0,goldbonus=~w where roleID=~w;",[NewGoldBonus,RoleID]),
	{ok,_Result}=db_sql:sql_execute_with_log(Sql),
	{Date, _} = Time = erlang:localtime(),
	%%添加gold消耗日志
	behavior_gold_consume:log(RoleID, VipLevel, 0, OldGold, OldGold, OldGoldBonus, Date, Time, ?MONEY_DEC_TYPE_BATCH_TRANSFORM, 0, "",[]),
	%%添加gold_bonus添加日志
	behavior_gold_bonus_add:log(RoleID, VipLevel, OldGold, OldGoldBonus, Date, Time, ?MONEY_ADD_TYPE_BATCH_TRANSFORM, 0, ""),
	%%添加操作日志
	db_sql:add_gold2goldbonus_log(RoleID,OldGold,OldGoldBonus,SrcType,0,NewGoldBonus,SrcType,?MONEY_DEC_TYPE_BATCH_TRANSFORM,0).

test_send_msg_to_family(Msg)->
	FamilyIDList = db_sql:get_all_family_id(),
	lists:foreach(fun(FamilyID)->
		family_misc:router_to_family_process(FamilyID,Msg)
	end,FamilyIDList).

scan_config(FileName,Prefix)->
	RealFile = 
		case Prefix of
			[]->
				FileName;
			_->
				Prefix++"/"++FileName
		end,
	case file:read_file_info(RealFile) of
		{ok,#file_info{type=FileType}}->
			case FileType of
				directory->
					scan_config2(RealFile);
				_ ->
					case string:right(FileName,6) =:= "config" of
						true->
							[{Prefix,FileName}];
						false->
							[]
					end
			end;
		Result->
			io:format("read file:~w failed:Result:~w  ~n",[RealFile,Result]),
			[]
	end.
scan_config2(Dir)->
	{ok,FileList} = file:list_dir_all(Dir),
	lists:foldl(fun(File,Acc)->
		Acc++scan_config(File,Dir)
	end,[],FileList).

fix_preload_config()->
	{ok,FD} = file:open("include/preload_config.hrl",[read]),
	ConfigList = scan_config("config",[]),
	NewData = fix_preload_config2(FD,[],ConfigList),
	file:write_file("include/preload_config.hrl",list_to_binary(NewData)),
	file:close(FD).
fix_preload_config2(FD,Acc,ConfigList)->
	case file:read_line(FD) of
		{ok,Data}->
			NewData = change_data(Data,ConfigList),
			fix_preload_config2(FD,Acc++NewData,ConfigList);
		_ ->
			io:format("read file finish~n"),
			Acc
	end.
change_data(Data,ConfigList)->
	case lists:member($%,Data) andalso hd(Data)=:=$% of
		true->
			%%注释
			Data;
		false-> 
			case string:tokens(Data,",") of
				[A,B|T] =E->
				NB = string:strip(B),
				case lists:keyfind(NB++".config",2,ConfigList) of
					false->
						Data;
					{Path,_}->
						string:join(["{\""++Path++"/"++NB++".config\"",B|T],",")
				end;
				_ ->
					Data
			end
	end.

test_finish_battle(RoleID)->
    Chapter = 
        #chapter{id = 1100,perfectRewarded = false,
             dungeonList = [#p_dungeon{dungeonID = 31000,restTimes = 10,
                                       bestScore = 3,resetTimes = 3},
                            #p_dungeon{dungeonID = 30829,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30828,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30827,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30826,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30825,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30824,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30823,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30822,restTimes = 30,bestScore = 3,
                                       resetTimes = 0},
                            #p_dungeon{dungeonID = 30821,restTimes = 30,bestScore = 3,
                                       resetTimes = 0}],
             curDate = {2017,2,13},
             starRewarded = 7},
    role_lib:send_server(RoleID, {test_set_chapter,Chapter,31000}).

reset_transmigration(RoleID)->
    role_lib:send_server(RoleID, {route, role_gm, {set_level, 300}}),
    role_lib:send_server(RoleID, reset_transmigration).

test_sc_msg(RoleID,Msg)->
  case catch role_lib:send_server(RoleID, {test_sc_msg,Msg}) of
    {'EXIT',_}->
      ?INFO("test_sc_msg(~w):~w",[RoleID,Msg]);
    _ ->
      ignore                            
  end.

test_clear_ai_flag(RoleID)->
    role_lib:send_server(RoleID, test_clear_ai_flag).

check_cross_serveres()->
    ets:tab2list(?ETS_NODE_INFO_TABLE).



%%===========================================检查阿努比斯是否有未布阵情况=================================================
create_anubis_sign_result_file_name()->
	{{Y,M,D},_} = erlang:localtime(),
	lists:flatten(io_lib:format("anubis_check_result_~..0w~2..0w~2..0w.config",[Y,M,D])).
check_has_anubis_sign(FamilyTimePairsList)->
	List = lists:foldl(fun({FamilyID,_Time}=Pairs,Acc)->
		ServerID = (FamilyID div 1000000)-1,
		case lists:keytake(ServerID,1,Acc) of
			false->
				[{ServerID,[Pairs]}|Acc];
			{_V,{ServerID,OtherPairs},Other}->
				[{ServerID,[Pairs|OtherPairs]}|Other]
		end
	end,[],FamilyTimePairsList),
	check_has_anubis_sign(List,[]).

check_has_anubis_sign([],Acc)->
	ResultFileName  = create_anubis_sign_result_file_name(),
	file:write_file(ResultFileName,io_lib:format("~p ~n",[Acc]));
check_has_anubis_sign([H|T],Acc)->
	Result = check_single_has_anubis_sign(H),
	check_has_anubis_sign(T,Result++Acc).

generate_log_filename({Year,Mon,Day})->
	lists:flatten(io_lib:format("log/error_log_~..0w_~..0w_~..0w.html",[Year,Mon,Day])).
	
check_single_has_anubis_sign({ServerID,Pairs})->
    case node_info_server:get_node_info(ServerID) of
            ignore->
                ?ERR("can not find Node for serverID:~w ~n",[ServerID]),
            	[{ServerID,node_ignore}];
            Node ->
                rpc:call(Node,user_default,check_single_has_anubis_sign2,[Pairs])
    end.

check_single_has_anubis_sign2(Pairs)->
	L = lists:foldl(fun({FamilyID,Time},Acc)->
		case lists:keytake(Time,2,Acc) of
			false->
				[{[FamilyID],Time}|Acc];
			{_V,{OtherFamilyID,Time},Other}->
				[{[FamilyID|OtherFamilyID],Time}|Other]
		end
	end,[],Pairs),
	lists:foldl(fun({FamilyIDList,Time},Acc)->
		Result = check_single_has_anubis_sign3(FamilyIDList,Time),
		Result++Acc
	end,[],L).

check_single_has_anubis_sign3(FamilyIDList,Time)->
	LogFileName = generate_log_filename(Time),
	case file:open(LogFileName,[raw,binary]) of
		{ok,Fd}->
			UndisFamilyList = find_all_undis(Fd,[]),
			?ERR("UndisFamilyList:~w Time:~w FamilyIDList:~w ~n",[UndisFamilyList,Time,FamilyIDList]),
			[{FamilyID,lists:member(FamilyID,UndisFamilyList),Time}||FamilyID<-FamilyIDList];
		Result->
			[{FamilyID,Result}||FamilyID<-FamilyIDList]
	end.

find_all_undis(Fd,Acc)->
	case file:read_line(Fd) of
		eof->
			Acc;
		{ok,Line}->
			case re:run(Line,"no dis player, but signed:(\\d*)",[{capture,[1],list}]) of
				nomatch->
					find_all_undis(Fd,Acc);
				{match,[Find]}->
					find_all_undis(Fd,[list_to_integer(Find)|Acc])
			end
	end.

clean_unread_mail()->
    L = db_sql:get_all("select mailUID from gMail where mailUID > ((select max(mailUID) from gMail limit 1) - 800000) and mailTemplateID in (1020,1021,1022) and time > 1503547200  and time < 1503550800"),
    ?ERR("delete MailList:~w",[L]),
    db_sql:del_mail([X||[X]<-L]).

%%删除已读的邮件
delete_read_mail(DeadlineTime) ->
	delete_read_mail(DeadlineTime, ?TRUE).
%%删除IsRead的邮件
%%IsRead::true已读|false未读|any所有
delete_read_mail(DeadlineTime, IsRead) when is_integer(DeadlineTime) ->
	delete_read_mail_1(DeadlineTime, IsRead);
delete_read_mail(OtherTime, IsRead) ->
	case catch util:datetime_to_seconds(OtherTime) of
		DeadlineTime when is_integer(DeadlineTime) -> delete_read_mail(DeadlineTime, IsRead);
		Why ->
			?ERR("delete_read_mail OtherTime=~p,Why=~p~n", [OtherTime, Why])
	end.
delete_read_mail_1(Time, IsRead) ->
	SelectSql = case is_boolean(IsRead) of
		    ?TRUE ->
			    io_lib:format("select mailUID from gMail where isRead = ~w and time < ~w limit 10", [db_sql:bool2int(IsRead), Time]);
		    _ -> io_lib:format("select mailUID from gMail where time < ~w limit 10", [Time])
	    end,
	L = db_sql:get_all(SelectSql),
	case L of
		[] ->
			?ERR("finish delete_read_mail"),
			ok;
		_ ->
			?ERR("delete readed MailList:~p", [L]),
			Func = fun([MailUID]) ->
				Sql = io_lib:format("delete from gMail where mailUID = ~w;", [MailUID]),
				db_sql:sql_execute_with_log(Sql)
			       end,
			lists:foreach(Func, L),
			delete_read_mail_1(Time, IsRead)
	end.

%% 删除TimeStamp前的邮件,循环每次清理N条
delete_outdate_mail(N,TimeStamp) ->
    case check_mail_outdate(1,TimeStamp) of
        false -> ignore;
        _ -> spawn(fun()-> 
                           Pid=self(),
                           erlang:register(delete_outdate_mail, Pid),
                           delete_early_mailUIDs(N,TimeStamp)
                    end)
    end.

check_mail_outdate(Pos,TimeStamp) ->
    N = if Pos > 0 -> Pos-1;true -> Pos end,
    case db_sql:get_row("select mailUID,time from gMail order by mailUID limit "++integer_to_list(N)++",1") of
        [UID,MailTime]  -> if TimeStamp < MailTime -> false;
                              true -> {true,UID}
                           end;
        _-> false
    end.
                             
delete_early_mailUIDs(N,TimeStamp) ->
    case check_mail_outdate(N,TimeStamp) of
        false -> ignore;
        {true,MailUID} -> 
            erlang:put(uid,MailUID),
            Sql = io_lib:format("delete from gMail where mailUID < ~w",[MailUID]),
            ?ERR("~s",[Sql]),
            db_sql:sql_execute_with_log(Sql),
            timer:sleep(100),
            delete_early_mailUIDs(N,TimeStamp)
    end.
    

%%====================================================================================================
%%==========================================自动生成更新链接文件======================================================================
%%链接文件的每项需要三个数据projectID，projectName，projectlink,如果projectID唯一或者所有projectID对应同一个projectlink，则projectID不填
collect_link(FileName)->
	case file:open(FileName,[read]) of
		{ok,FD}->
			collect_link2(FD,[]);
		R->
			?ERR("open file:~p failed reason:~w ~n",[FileName,R])
	end.
collect_link2(FD,Acc)->
	case file:read_line(FD) of
		eof->
			?ERR("read file finish ~n"),
			Acc;
		{ok,Data}->
			case string:tokens(Data,"\t\n") of
				%%三个值都存在
				[A,B,C]->
					collect_link2(FD,[{A,B,C}|Acc]);
				%%不存在projectID的情况
				[A,B]->
					collect_link2(FD,[{unique,A,B}|Acc]);
				_ ->
					?ERR("not find link link:~p~n",[Data]),
					collect_link2(FD,Acc)
			end
	end.

update_package_file(PackageFile,LinkFile,PreVersion,NewVersion,UpdateType)->
	LinkList = collect_link(LinkFile),
	PackageList = case file:consult(PackageFile) of
		{ok,Data}->
			Data;
		{error,R}->
			?ERR("not consult file:~p reason:~p~n",[PackageFile,R])
	end,
	generate_new_package_file(PackageList,LinkList,PreVersion,NewVersion,PackageFile,UpdateType).

generate_new_package_file(PackageList,LinkList,PreVersion,NewVersion,PackageFile,UpdateType)->
	NewPackageList = lists:foldl(fun({ProjectID,PackageName,PackageLink}=E,Acc)->
		case lists:keytake(PackageName,1,Acc) of
			false->
				?ERR("LinkElement:~p is not find in PackageFile~n",[E]),
				Acc;
			{_Value,F,Other}->
				{PackageName,PackageLinkList} = F,
				NewLinkList = change_single_package_linklist(ProjectID,PackageLink,PackageLinkList,PreVersion,NewVersion,UpdateType),
				[{PackageName,NewLinkList}|Other]
		end
	end,PackageList,LinkList),
	DesFileName = generate_new_package_file_name(PackageFile),
	% file:write_file(DesFileName,io_lib:format("~p",[NewPackageList])).
	{ok,DesFD} = file:open(DesFileName,[write,append]),
	[file:write(DesFD,io_lib:format("~p.~n",[E]))||E<-lists:reverse(NewPackageList)],
	file:close(DesFD).

generate_new_package_file_name(PackageFile)->
	{{Y,M,D},{H,Min,S}} = erlang:localtime(),
	lists:flatten(PackageFile++io_lib:format("_~4..0w~2..0w~2..0w~2..0w~2..0w",[Y,M,D,H,Min])).

change_single_package_linklist(ProjectID,Link,PackageLinkList,PreVersion,NewVersion,UpdateType)->
	[change_linklist_for_single_projectID(E,Link,PreVersion,NewVersion,UpdateType,ProjectID)||E<-PackageLinkList].

change_linklist_for_single_projectID(LinkForSingleProjectID,Link,PreVersion,NewVersion,UpdateType,ProjectID)->
	{PreProjectID,PreLinkVersion,_PreUpdateType,_PreLink} = LinkForSingleProjectID,
	case PreLinkVersion=:=PreVersion andalso (PreProjectID=:=ProjectID orelse ProjectID=:=unique) of
		true->
			{PreProjectID,NewVersion,UpdateType,Link};
		false->
			LinkForSingleProjectID
	end.
%%==========================================自动生成更新链接文件 END======================================================================

%%=========================跨服发放奖励===============================================
%%增加一个跨区服邮件发送奖励接口
test_send_reward_batch(L)->
	lists:foreach(fun({ServerID,RoleRewardL})->
		case node_info_server:get_node_info(ServerID) of
			ignore->
				?ERR("can not find Node for ServerID:~w ~n",[ServerID]),
				?ERR("Send RewardList:~w ~n",[RoleRewardL]);
			Node ->
				rpc:call(Node,user_default,test_send_reward_batch2,[RoleRewardL])
		end
	end,L).

test_send_reward_batch2(L)->
	lists:foreach(fun({RoleID,Reward})->
		mail_server:send_sys_mail(RoleID,0, [],"", Reward)
	end,L).

merge_reward_by_serverID([],Acc)->
	Acc;
merge_reward_by_serverID([{RoleID,_Reward}=H|T],Acc)->
	TarServerID = (RoleID div 1000000)-1,
	case lists:keytake(TarServerID,1,Acc) of
		false->
			merge_reward_by_serverID(T,[{TarServerID,[H]}|Acc]);
		{_V,{ServerID,L},OtherS}->
			merge_reward_by_serverID(T,[{ServerID,[H|L]}|OtherS])
	end.
%%=======================跨服发奖结束==================================================
get_diamond(RoleID)->
    DL = db_sql:get_gerHolyGrail(RoleID),
    DC = lists:foldl(fun({_,#holyGrail{diamondInfo=GerDiamondInfo}},ACC) ->
                             lists:foldl(fun(#diamond_unit{diamondID=DiamondID},Ac)->
                                                 case lists:keytake(DiamondID, 1, Ac) of
                                                     false -> [{DiamondID,1}|Ac];
                                                     {value,{_,C2},Ac2} -> [{DiamondID,C2+1}|Ac2]
                                                 end
                                         end, ACC, GerDiamondInfo)
                     
                     end, [], DL),
   Sql = io_lib:format("select itemUID,itemTypeID,itemNum from gBagItem where roleID = ~w and itemTypeID >= 3101 and itemTypeID <= 3817;",[RoleID]),
   CC = 
   case db_sql:get_all(Sql) of
       [] -> DC;
       L -> 
           L2 =[{A,B,C}||[A,B,C]<-L], 
               lists:foldl(fun({ItemTypeID,ItemNum},BagAcc) ->
                                case lists:keytake(ItemTypeID, 2, BagAcc) of
                                    false -> [{0,ItemTypeID,ItemNum}|BagAcc];
                                    {value,{ItemUid,_,BagNum},BagAcc2} -> [{ItemUid,ItemTypeID,ItemNum+BagNum}|BagAcc2]
                                end
                        end, L2, DC)
   end,
   CC.

flush_diamond_info() ->
    timer:sleep(random:uniform(10000)),
    %AllRole = db_sql:get_all_roles(),%db_sql:get_all("select roleID from gGerHolyGrail;"),
    AllRole = db_sql:get_level_offtime_roles(10,1000,1505923200,1508515200),
    {ok,File} = file:open("log/log_370_gerHolyGrail.txt", [write,raw]),
    lists:foreach(fun(RoleID)->
                          CC = get_diamond(RoleID),
                          case CC of [] -> ignore;
                              _ ->
                          Str = io_lib:format("{~w,~w}.\n",[RoleID,CC]),
                          file:write(File,Str)
                          end,
                          timer:sleep(random:uniform(8))
                          end, AllRole),
    file:close(File),
    ok.


%%=======================================获取玩家当前的晶体和觉醒信息==================================
get_ger_info(RoleList)->
	{ok,AwakeFile} = file:open("log/log_370_awakeinfo.config",[append,raw]),
	{ok,CrystalFile} = file:open("log/log_370_crystal.config",[append,raw]),
	get_ger_info(RoleList,AwakeFile,CrystalFile).

get_ger_info([],AwakeFile,CrystalFile)->
	file:close(AwakeFile),
	file:close(CrystalFile);

get_ger_info([RoleID|T],AwakeFile,CrystalFile)->
	{AwakeInfo,CrystalInfo} = get_ger_info2(RoleID),
	AwakeStr = io_lib:format("{~w,~w}.\n",[RoleID,AwakeInfo]),
	file:write(AwakeFile,AwakeStr),
	CrystalStr = io_lib:format("{~w,~w}.\n",[RoleID,CrystalInfo]),
	file:write(CrystalFile,CrystalStr),
	get_ger_info(T,AwakeFile,CrystalFile).

get_ger_info2(RoleID)->
	AwakeInfo = get_ger_awake_info(RoleID),
	CrystalInfo = get_ger_crystal_info(RoleID),
	{AwakeInfo,CrystalInfo}.

get_ger_awake_info(RoleID)->
	Sql = io_lib:format("select gerID,gerTypeID,gerAwakeInfo from gGer where roleID=~w and gerTypeID>7000;",[RoleID]),
	case db_sql:get_all(Sql) of
		[]->
			[];
		L ->
			lists:foldl(fun([GerID,GerTypeID,AwakeBin],Acc)->
					AwakeInfo = db_sql:to_term(AwakeBin),
					case AwakeInfo of
						[]->
							Acc;
						_->
							[{GerID,GerTypeID,to_simple_awake(AwakeInfo)}|Acc]
					end
				end,[],L)
	end.

get_ger_crystal_info(RoleID)->
	L = db_sql:get_gerCrystal(RoleID),
	[{GerID,to_simple_crystal(C)}||{GerID,C}<-L,C=/=-1].


test_get_ger_info(RoleIDList)->
	MergeRoleList = merge_role_by_server(RoleIDList,[]),
	lists:foreach(fun({ServerID,RoleList})->
		case node_info_server:get_node_info(ServerID) of
			ignore->
				?ERR("can not find Node for ServerID:~w ~n",[ServerID]),
				?ERR("Send RewardList:~w ~n",[RoleList]);
			Node ->
				rpc:call(Node,user_default,get_ger_info,[RoleList])
		end
	end,MergeRoleList).

merge_role_by_server([],Acc)->
	Acc;
merge_role_by_server([RoleID|T],Acc)->
	TarServerID = (RoleID div 1000000)-1,
	case lists:keytake(TarServerID,1,Acc) of
		false->
			merge_role_by_server(T,[{TarServerID,[RoleID]}|Acc]);
		{_,{TarServerID,ORL},Other}->
			merge_role_by_server(T,[{TarServerID,[RoleID|ORL]}|Other])
	end. 

to_simple_crystal(L) when is_list(L)->
	[to_simple_crystal(E)||E<-L];
to_simple_crystal({_,A,B,C,D,E})->
	{A,B,C,D,E}.

to_simple_awake(L) when is_list(L)->
	[to_simple_awake(E)||E<-L];
to_simple_awake({_,A,B,C,D,E})->
	{A,B,C,D,E}.

%%=====================================================================统计晶体===================================================
analyse_crystal(File)->
	io:format("~p ~n",[File]),
	{ok,Terms} = file:consult(File),
	analyse_role_crystal_term(Terms,[]).

analyse_role_crystal_term([],Acc)->
	Acc;
analyse_role_crystal_term([{RoleID,GerList}|T],Acc)->
	{SExp,SRankExp} = analyse_ger_crystal_term(GerList,{0,0}),
	analyse_role_crystal_term(T,[{RoleID,SExp,SRankExp}|Acc]).

analyse_ger_crystal_term([],Acc)->
	Acc;
analyse_ger_crystal_term([H|T],{ExpAcc,RankExpAcc})->
	{AddExp,AddRankExp} = analyse_single_ger_crystal_term(H),
	analyse_ger_crystal_term(T,{ExpAcc+AddExp,RankExpAcc+AddRankExp}).

analyse_single_ger_crystal_term({GerID,CrystalList})->
	lists:foldl(fun({Type,Quality,Level,Exp,RExp},{ExpAcc,RankExpAcc})->
		{AddExp,AddRankExp} = role_crystal:calculate_crystal_exp_cost(Type,Quality,Level),
		{ExpAcc+AddExp+Exp,RankExpAcc+AddRankExp+RExp}
	end,{0,0},CrystalList).

analyse_crystal_dir(Dir)->
	{ok,FileList} = file:list_dir_all(Dir),
	analyse_crystal_dir(FileList,[],Dir).
analyse_crystal_dir([],Acc,_Dir)->
	file:write_file("crystal_statistic.config",io_lib:format("~w~n",[Acc]),[write]);
analyse_crystal_dir([File|T],Acc,Dir)->
	AddData =  analyse_crystal(Dir++"/"++File),
	analyse_crystal_dir(T,Acc++AddData,Dir).
%%================================================================统计晶体结束=====================================================

%%==================================================================统计觉醒=======================================================
analyse_awake(File)->
	{ok,Terms} = file:consult(File),
	analyse_role_awake_term(Terms,[]).

analyse_role_awake_term([],Acc)->
	Acc;
analyse_role_awake_term([{RoleID,GerList}|T],Acc)->
	GerAwakeInfo = analyse_ger_awake_term(GerList,[],RoleID),
	analyse_role_awake_term(T,[GerAwakeInfo|Acc]).

analyse_ger_awake_term([],Acc,_RoleID)->
	Acc;
analyse_ger_awake_term([{GerID,GerTypeID,AwakeList}|T],Acc,RoleID)->
	AwakeStep = [Step||{Step,_,_,_,_}<-AwakeList],
	analyse_ger_awake_term(T,[{RoleID,GerTypeID,erlang:max(length(AwakeStep),lists:max(AwakeStep))}|Acc],RoleID).

analyse_awake_dir(Dir)->
	{ok,FileList} = file:list_dir_all(Dir),
	analyse_awake_dir(FileList,[],Dir).
analyse_awake_dir([],Acc,_Dir)->
	file:write_file("awake_statistic.config",io_lib:format("~w~n",[Acc]),[write]);
analyse_awake_dir([File|T],Acc,Dir)->
	AddData =  analyse_awake(Dir++"/"++File),
	analyse_awake_dir(T,Acc++AddData,Dir).

%%================================================================统计觉醒结束====================================================