%% @author caohongyang
%% @doc 战报进程
%% Created 2013-5-24

-module(hist_server).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_hist.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).

-export([hist_pvp/7,hist_plunder/6]).

hist_pvp(SrcRoleID, SrcName, TarRoleID, TarRoleName, FightInfo,SrcNewRank,TarNewRank) ->
	erlang:send(?MODULE, {log_pvp_fight,SrcRoleID, SrcName, TarRoleID, TarRoleName, FightInfo,SrcNewRank,TarNewRank}).

hist_plunder(SrcRoleID, SrcName, TarRoleID, FightInfo, PatchID, IsGetPatch) ->
	%% @todo 过滤机器人
	case tk_id:is_robot(TarRoleID) of
		true ->
			ignore;
		false ->
			erlang:send(?MODULE, {log_plunder, SrcRoleID, SrcName, TarRoleID, FightInfo, PatchID, IsGetPatch})
	end.

clear_hist(RoleID)->
	erlang:send(?MODULE,{clear_hist,RoleID}).
role_login(RoleID)->
	erlang:send(?MODULE,{role_login,RoleID}).

-define(DUMP_INTERVAL, 10).%写数据库的间隔,单位：秒
-define(TICK_INTERVAL, 60). % 检查进程buff大小的时间间隔，单位：秒
-define(HIST_SYNC_NUM, 10).% 一次发送的战报数量
-define(MAX_HIST_NUM, 30).% 最大战报数量
%% ===================Dict Key Begin =========================
-define(READ_LIST, read_list).%进程字典中缓存起来的等待写数据库的已读标记
-define(DEL_LIST, del_list).%进程字典中缓存起来的等待写入删除记录的列表
-define(ADD_LIST, add_list).%进程字典缓存起来的，新增邮件，等待写入数据库
-define(offlineList,offlineList). % 下线列表
%% ===================Dict Key End   =========================

i() ->
	gen_server:call(?MODULE, i).

start() ->
	{ok,_}=
    supervisor:start_child(world_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:init/1	
init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	tick(),
	dump_tick(),
    {ok, ?undefined}.


-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% @doc gen_server:init/1
handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_cast/2
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% @doc gen_server:handle_info/2
handle_info(tick, State) ->
	{memory,Memory }= erlang:process_info(self(),memory),
	MemoryByM = Memory div (1024*1024),
	MaxBuffSizeByM = data_setting:get(hist_process_max_buff_size),
	if MemoryByM >= MaxBuffSizeByM ->
		   clear_buff();
	   true ->
		   ignore
	end,
	erlang:garbage_collect(),
	tick(),
	{noreply, State, hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
    {noreply, State}.



-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% @doc gen_server:terminate/2
terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.



-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% @doc gen_server:code_change/3
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


transform_list(List) ->
	[hist2p_hist(E) || E<-List].
hist2p_hist(Hist) ->
	#p_hist{histUID=Hist#hist.histUID,
			histType=Hist#hist.histType,
			name=Hist#hist.name,
			roleID=Hist#hist.enemyID,
			time=Hist#hist.time,
			arg=Hist#hist.arg}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 客户端初始化邮件，或者第二次打开邮件界面时，请求同步新邮件
do_handle_info({client_msg, RoleID, #cs_hist_get_list{type=Type,topClientUID=TopClientUID}}) ->
	if Type =:=?TYPE_PVP orelse Type =:= ?TYPE_PLUNDER ->
		   if TopClientUID =:= 0 ->
				  #d_hist{histList=List,unreadNum=UN} = HistInfo = get_hist(RoleID,Type),
				  {SendList,List2,DecUN, NewReadList} = init_client_hist(List),	
				  add_readList(NewReadList),
				  if DecUN == 0 ->
						 UN2=UN,
						 ignore;
					 true ->
						 UN2 = erlang:max(0,UN-DecUN),
						 HistInfo2 = HistInfo#d_hist{histList=List2,unreadNum=UN2},
						 set_hist(RoleID, Type, HistInfo2)
				  end,
				  SendList2 = transform_list(SendList),
				  ?unicast(RoleID,#sc_hist_get_list{type=Type,historyList=SendList2,isDiscardData=false,unreadNum=UN2});
			  true ->
				  #d_hist{histList=List,unreadNum=UN} = HistInfo = get_hist(RoleID,Type),
				  {List2, SendList, DecUN, IsDiscardData, NewReadList} = sync_hist(List, TopClientUID),
				  add_readList(NewReadList),
				  if DecUN == 0 ->
						 UN2=UN,
						 ignore;
					 true ->
						 UN2 = erlang:max(0,UN-DecUN),
						 HistInfo2 = HistInfo#d_hist{histList=List2,unreadNum=UN2},
						 set_hist(RoleID, Type, HistInfo2)
				  end,
				  SendList2 = transform_list(SendList),
				  ?unicast(RoleID,#sc_hist_get_list{type=Type,historyList=SendList2,isDiscardData=IsDiscardData,unreadNum=UN2})
		   end;
	   true ->
		   ?unicast(RoleID, #sc_hist_get_list{type=Type,historyList=[]})
	end;	
do_handle_info({client_msg, RoleID, #cs_hist_more{type=Type,histUID=HistUID}}) ->
	if Type =:=?TYPE_PVP orelse Type =:= ?TYPE_PLUNDER ->
			#d_hist{histList=List,unreadNum=UN} = HistInfo = get_hist(RoleID,Type),
		   case more_hist(List, HistUID) of
			   false ->
				   ?unicast(RoleID, #sc_hist_more{type=Type,historyList=[],unreadNum=UN});
			   {List2,SendList,DecUN, NewReadList} ->
				   add_readList(NewReadList),
				  if DecUN == 0 ->
						 UN2=UN,
						 ignore;
					 true ->
						 UN2 = erlang:max(0,UN-DecUN),
						 HistInfo2 = HistInfo#d_hist{histList=List2,unreadNum=UN2},
						 set_hist(RoleID, Type, HistInfo2)
				  end,
				   SendList2 = transform_list(SendList),
				   ?unicast(RoleID,#sc_hist_more{type=Type,historyList=SendList2, unreadNum=UN2})
		   end;		   
	   true ->
		   ?unicast(RoleID, #sc_hist_more{type=Type,historyList=[], unreadNum=0})
	end;
do_handle_info({client_msg, RoleID, #cs_hist_replay{histUID=HistUID, type=Type}}) ->
	if Type =:=?TYPE_PVP orelse Type =:= ?TYPE_PLUNDER ->
		   case get_fightInfo(HistUID) of
			   [] ->
				   ?unicast(RoleID,#sc_hist_replay{result=2,fightInfo=[]});
			   FightInfo ->
				   ?unicast(RoleID, #sc_hist_replay{result=1,fightInfo=[FightInfo]})
		   end;
	   true ->
		   ?unicast(RoleID,#sc_hist_replay{result=2,fightInfo=[]})		   
	end;
do_handle_info({log_pvp_fight,SrcRoleID, SrcRoleName, TarRoleID, TarRoleName, FightInfo,SrcNewRank,TarNewRank}) ->
	Result = FightInfo#sc_fight_request.result,
	if Result ->
		   WinType = ?HIST_WIN;
	   true ->
		   WinType = ?HIST_FAIL
	end,
	add_hist(SrcRoleID, TarRoleID, TarRoleName, FightInfo, SrcNewRank, WinType bor ?HIST_PVP bor ?HIST_YOU_FIGHT, ?TYPE_PVP),
	%% 过滤机器人
	case tk_id:is_robot(TarRoleID) of
		true ->
			ignore;
		false ->
			add_hist(TarRoleID, SrcRoleID, SrcRoleName, FightInfo, TarNewRank, WinType bor ?HIST_PVP bor ?HIST_YOU_BE_FIGHT, ?TYPE_PVP)
	end;
do_handle_info({log_plunder, SrcRoleID, SrcName, TarRoleID, FightInfo, PatchID, IsGetPatch}) ->
	Result = FightInfo#sc_fight_request.result,
	if Result ->
		   WinType = ?HIST_WIN;
	   true ->
		   WinType = ?HIST_FAIL
	end,
	if IsGetPatch ->
		   GetPatchType = ?HIST_SUCC_PATCH;
	   true ->
		   GetPatchType = ?HIST_FAIL_PATCH
	end,
	add_hist(TarRoleID, SrcRoleID, SrcName, FightInfo,PatchID, 
			 WinType bor GetPatchType bor ?HIST_PLUNDER bor ?HIST_YOU_BE_FIGHT , ?TYPE_PLUNDER);
do_handle_info({clear_hist,RoleID})->
	add_offlineList(RoleID);
do_handle_info({role_login,RoleID})->
	del_offlineList(RoleID);
do_handle_info(dump_tick) ->
	?CATCH(do_write_db()),
	dump_tick();
do_handle_info(Info) ->
	throw({cannot_handle,Info}).

del_offlineList(RoleID)->
	set_offlineList(lists:delete(RoleID, get_offlineList())).
add_offlineList(RoleID)->
	set_offlineList([RoleID|get_offlineList()]).
set_offlineList(List)->
	erlang:put(?offlineList,List).
get_offlineList()->
	case erlang:get(?offlineList) of
		?undefined ->
			[];
		X ->
			X
	end.
	
add_hist(RoleID, HisRoleID, HisRoleName, FightInfo, Arg, HistType, Type) ->
	HistUID = tk_id:gen_mailUID(),
	NowSec = util:now(),
	Hist = #hist{arg=Arg,histType=HistType,histUID=HistUID,name=HisRoleName,enemyID=HisRoleID,time=NowSec,isRead=false},
	case role_lib:is_online(RoleID) of
		true ->
			#d_hist{histList=List,unreadNum=UN} = HistInfo = get_hist(RoleID, Type),
			if length(List) >= ?MAX_HIST_NUM ->
				   {List2,DelList} = lists:split(?MAX_HIST_NUM-1, List),
				   DelIDList = [E#hist.histUID||E<-DelList],
				   add_delList(DelIDList),
				   DelUN = lists:foldl(fun(E,Acc) -> 
											   if E#hist.isRead =:= false ->
													  Acc+1;
												  true ->
													  Acc
											   end
									   end, 0, DelList),
				   UN2 = UN +1 -DelUN,
				   List3 = [Hist|List2];
			   true ->
				   List3 = [Hist|List],
				   UN2 = UN +1
			end,
			HistInfo2 = HistInfo#d_hist{histList=List3,unreadNum=UN2},
			set_hist(RoleID, Type, HistInfo2),		
			?unicast(RoleID, #sc_hist_unreadNum{type=Type,unreadNum=UN2});
		_ ->
			ignore
	end,
	add_addList(RoleID, Hist, FightInfo, Type).

get_hist(RoleID,Type) ->
	case erlang:get({RoleID,Type}) of
		#d_hist{}=HistInfo ->
			HistInfo;
		_ ->
			HistInfo = get_db_hist(RoleID, Type),
			set_hist(RoleID, Type, HistInfo),
			HistInfo
	end.

get_db_hist(RoleID,Type) ->
	db_sql:get_histList(RoleID, Type).

set_hist(RoleID, Type, HistInfo) ->
	Key = {RoleID, Type},
	erlang:put(Key, HistInfo).

init_client_hist(CerMailList) ->
	init_client_hist(CerMailList, [], 0, 0, []).
init_client_hist([], Send, _SendNum, DecNum, NewReadList) ->
	SendMail = lists:reverse(Send),
	{SendMail, SendMail, DecNum, NewReadList};
init_client_hist(List, Send, ?HIST_SYNC_NUM, DecNum, NewReadList) ->
	SendMail = lists:reverse(Send),
	{SendMail, SendMail ++ List, DecNum, NewReadList};
init_client_hist([M|List], Send, SendNum, DecNum, NewReadList) ->
	#hist{isRead=IsRead,histUID=HistUID} = M,
	if IsRead=:=true->
		   init_client_hist(List, [M|Send], SendNum+1, DecNum, NewReadList);
	   true ->
		   init_client_hist(List, [M#hist{isRead=true}|Send], SendNum+1, DecNum+1, [HistUID|NewReadList])
	end.


sync_hist(CerMailList, ClientTopUID) ->
	sync_hist(CerMailList, [], 0, 0, ClientTopUID,[]).
sync_hist([], Send, _SendNum, DecNum, _ClientTopUID, NewReadList) ->
	SendMail = lists:reverse(Send),
	{SendMail,SendMail,DecNum,false, NewReadList};
sync_hist(List, Send, ?HIST_SYNC_NUM,DecNum,ClientTopUID, NewReadList) ->
	SendMail = lists:reverse(Send),
	DoDiscardData=
		case List of
			[#mail{mailUID=ClientTopUID}|_] ->
				true;
			_ ->
				false
		end,
	{SendMail++List, SendMail, DecNum, DoDiscardData, NewReadList};		   
sync_hist([M|List], Send, SendNum, DecNum, ClientTopUID, NewReadList) ->
	if M#hist.histUID=:= ClientTopUID ->
		   
		   SendMail = lists:reverse(Send),
		   {SendMail++List, SendMail, DecNum, false, NewReadList};		   
	   true ->
		   if M#hist.isRead =:= true ->
		   sync_hist(List, [M|Send], SendNum+1,DecNum, ClientTopUID, NewReadList);
			  true ->				  
		   sync_hist(List, [M#hist{isRead=true}|Send], SendNum+1,DecNum+1, ClientTopUID, [M#hist.histUID|NewReadList])
		   end
	end.

more_hist(CerMailList, StartUID) ->
	more_hist(CerMailList, [], StartUID).
more_hist([], _Rev, _StartUID) ->
	false;
more_hist([M|List],Rev, StartUID) ->
	if M#hist.histUID=:= StartUID ->
		   more_hist2(List, [M|Rev], [], 0, 0, []);
	   true ->
		   more_hist(List, [M|Rev], StartUID)
	end.

more_hist2([], Rev, Send, _SendNum, DecNum, NewReadList) ->
	{lists:reverse(Rev), lists:reverse(Send), DecNum, NewReadList};
more_hist2(List, Rev, Send, ?HIST_SYNC_NUM, DecNum, NewReadList) ->
	{lists:reverse(Rev, List), lists:reverse(Send), DecNum, NewReadList};
more_hist2([M|List], Rev, Send, SendNum, DecNum, NewReadList) ->
	if M#hist.isRead =:=true->
		more_hist2(List, [M|Rev], [M|Send], SendNum+1, DecNum, NewReadList);
	   true ->
		   M2 = M#hist{isRead=true},
		   more_hist2(List, [M2|Rev],[M2|Send],SendNum+1,DecNum+1, [M#hist.histUID|NewReadList])
	end.

clear_buff() ->
	lists:foreach(fun({{RoleID,Type}=Key,#d_hist{}}) when is_integer(RoleID) andalso is_integer(Type) ->
						  erlang:erase(Key);
					 (_) ->
						  ignore
				  end, erlang:get()).

%% 检查本进程buff
tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).

%% 写数据库时间间隔
dump_tick() ->
	erlang:send_after(?DUMP_INTERVAL*1000, self(), dump_tick).


get_readList() ->
	case erlang:get(?READ_LIST) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.

set_readList(List) ->
	erlang:put(?READ_LIST, List).

add_readList([]) -> ignore;
add_readList(List) ->
	set_readList(List++get_readList()).

get_delList() ->
	case erlang:get(?DEL_LIST) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.
		
set_delList(List) ->
	erlang:put(?DEL_LIST, List).

add_delList([]) -> ignore;
add_delList(List) ->
	set_delList(List++get_delList()).
	
get_addList() ->
	case erlang:get(?ADD_LIST) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.
		
set_addList(List) ->
	erlang:put(?ADD_LIST, List).

add_addList(RoleID, Mail, FightInfo, Type) ->
	set_addList([{RoleID, Mail, FightInfo, Type}|get_addList()]).

clear_offlineList()->
	List=get_offlineList(),
	set_offlineList([]),
	lists:foreach(fun(E)->erlang:erase({E,?TYPE_PVP}),erlang:erase({E,?TYPE_PLUNDER}) end, List).
	
do_write_db() ->
	clear_offlineList(),
	db_sql:add_histList(get_addList()),
	set_addList([]),
	db_sql:read_hist(get_readList()),
	set_readList([]),
	db_sql:del_hist(get_delList()),
	set_delList([]).

get_fightInfo(MailUID) ->
	Cache = get_addList(),
	case util:fun_find(fun({_,Mail,_,_}) ->
							   Mail#hist.histUID== MailUID
					   end, Cache) of
		false ->
			db_sql:get_hist_fightInfo(MailUID);
		{_,_,FightInfo,_} ->
			FightInfo
	end.
	