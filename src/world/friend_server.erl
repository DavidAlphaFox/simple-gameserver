%% @author caohongyang
%% @doc 好友进程
%% Created 2013-6-3

-module(friend_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_hist.hrl").
-include("def_mail.hrl").
-include("def_homestead.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).

-export([add_friend/3,add_enemy/2,send_to_friend/2]).
-define(TYPE_PAL, 1). % 朋友
-define(TYPE_FOE, 2). % 敌人
-define(TICK_INTERVAL, 60). % 检查进程buff大小的时间间隔，单位：秒
-define(TICK_INTERVAL_REFRESH, 3600). % 更新 ?ETS_ROLE_LEVEL
-define(FRIEND_SYNC_NUM, 20).% 一次发送的战报数量
-define(MAX_FRIEND_NUM, 100).% 最大好友数量
-define(ADVISE_ROLE_NUM, 5).% 推荐好友数量
-define(ADDLIST_RANGE_INIT, 5). %% 生成添加好友的列表时，所用的范围参数
-define(ADDLIST_RANGE_TOTAL, 200). %% 生成添加好友的列表时，所用的范围参数
%% ===================Dict Key Begin =========================
%% -define(random_pool, random_pool).%所有玩家列表
%% ===================Dict Key End   ========================

%% 添加好友
add_friend(AgreeRoleID, RequestRoleID, MailUID) ->
	erlang:send(?MODULE, {add_friend, AgreeRoleID, RequestRoleID, MailUID}).

send_to_friend(RoleID,Record)->
	erlang:send(?MODULE,{send_to_friend,RoleID,Record}).

%% 添加仇人(暂时屏蔽)
add_enemy(_RoleID, _EnemyID) ->
%% 	erlang:send(?MODULE, {add_enemy, RoleID, EnemyID}).
	ok.
i() ->
	gen_server:call(?MODULE, i).

start() ->
    {ok,_}=supervisor:start_child(world_sup, 
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
    tick_refresh(),
    ets:insert(?ETS_ROLE_LEVEL, db_sql:get_roleIDList_noRobot(15)), 
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
handle_call({get_role_friend_roleID,Type,RoleID},_From,State)->
		#d_friend{pal=Pal,foe=Foe} = get_friend(RoleID),
		Reply = case Type of
			?TYPE_PAL->
				Pal;
			?TYPE_FOE->
				Foe;
			_ ->
				?ERR("undefined Type:~w ~n",[Type]),
				[]
		end,
		{reply,Reply,State};
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

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info(tick, State) ->
	{memory,Memory }= erlang:process_info(self(),memory),
	MemoryByM = Memory div (1024*1024),
	MaxBuffSizeByM = data_setting:get(friend_process_max_buff_size),
	if MemoryByM >= MaxBuffSizeByM ->
		   clear_buff();
	   true ->
		   ignore
	end,
%% 	erlang:garbage_collect(),
	tick(),
	{noreply, State};
handle_info(tick_refresh, State) ->
    ?INFO("tick_refresh. to init ETS_ROLE_LEVEL"),
    ets:delete_all_objects(?ETS_ROLE_LEVEL),
    ets:insert(?ETS_ROLE_LEVEL, db_sql:get_roleIDList_noRobot(15)), 
    tick_refresh(),
    {noreply, State};
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

check_operate_homestead(RoleID,FriendRoleID)->
%% 	#d_friend{pal=Pal} = get_friend(RoleID),
%% 	case lists:member(FriendRoleID, Pal) of
%% 		true->
			#rolePublic{level=Level1} = role_lib:get_rolePublic(RoleID),
			case role_homestead:is_open_homestead(Level1) of
				false->
					{false,?HOMESTEAD_ERROR_NOT_OPEN};
				true->
					#rolePublic{level=Level2} = role_lib:get_rolePublic(FriendRoleID),
					case role_homestead:is_open_homestead(Level2) of
						false->
							{false,?HOMESTEAD_ERROR_FRIEND_NOT_OPEN};
						true->
							HomesteadInfo = homestead_server:get_ets_homestead_base(RoleID),
							MachineList = homestead_server:get_ets_homestead_machineList(RoleID),
							FriendHomesteadInfo = homestead_server:get_ets_homestead_base(FriendRoleID),
							FriendMachineList = homestead_server:get_ets_homestead_machineList(FriendRoleID),
							{true,HomesteadInfo,MachineList,FriendHomesteadInfo,FriendMachineList}
					end
			end.
%% 		false->
%% 			{false,?HOMESTEAD_ERROR_ONLY_REQUEST_FRIEND}
%% 	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({client_msg, RoleID,#cs_friend_send_enargy{roleID=FriendRoleID}})->
	#d_friend{pal=Pal} = get_friend(RoleID),
	case lists:member(FriendRoleID, Pal) of
		true->
			enargy_server:enargy_send(RoleID, FriendRoleID);
		false->
			?unicast(RoleID, #sc_friend_send_enargy{result=2})
	end;
do_handle_info({client_msg, RoleID,#cs_homestead_addenergy{roleID=FriendRoleID,num=MachineNum}})->
	case check_operate_homestead(RoleID,FriendRoleID) of
		{true,#p_homestead{energyTimes=EnergyTimes},_,_,FriendMachineList}->
			case homestead_server:check_homestead_addenergy(EnergyTimes, MachineNum, FriendMachineList) of
				{false,ErrorCode}->
					?unicast(RoleID, #sc_homestead_error{reason_code=ErrorCode});
				_->
					homestead_server:homestead_addenergy(RoleID, FriendRoleID, MachineNum)
			end;
		{false,ErrorCode}->
			?unicast(RoleID, #sc_homestead_error{reason_code=ErrorCode})
	end;
do_handle_info({client_msg, RoleID, #cs_homestead_mating{roleID=FriendRoleID}}) ->
	case check_operate_homestead(RoleID, FriendRoleID) of
		{true,#p_homestead{matingTimes=MatingTimes,gerID=GerID},_,#p_homestead{matingCoolSecond=FMatingCoolSecond,gerID=FGerID},_}->
			case homestead_server:check_homestead_mating(GerID,MatingTimes, FMatingCoolSecond, FGerID) of
				{false,ErrorCode}->
					?unicast(RoleID, #sc_homestead_error{reason_code=ErrorCode});
				_->
					homestead_server:homestead_mating(RoleID, FriendRoleID)
			end;
		{false,ErrorCode}->
			?unicast(RoleID, #sc_homestead_error{reason_code=ErrorCode})
	end;
do_handle_info({client_msg, RoleID, #cs_homestead_get_friend_info{roleID=FriendRoleID}}) ->
	case check_operate_homestead(RoleID, FriendRoleID) of
		{true,_,_,_,_}->
				HomesteadInfo = homestead_server:get_ets_homestead_base(FriendRoleID),
				case homestead_server:is_need_refresh_mating(HomesteadInfo#p_homestead.refreshMatingSecond) of
					false ->
						HomesteadInfo2=HomesteadInfo;
					true ->
						HomesteadInfo2=homestead_server:refresh_homestead_1(HomesteadInfo),
						homestead_server:update_ets_homestead_base(FriendRoleID, HomesteadInfo2)
				end,
				MachineList0 = homestead_server:get_ets_homestead_machineList(FriendRoleID),
                #rolePublic{level=FriendLevel} = role_lib:get_rolePublic(FriendRoleID),
                MachineList = 
                    lists:foldr(fun(Machine,AccList)->
                            #p_homestead_machine{seedItemID = SeedItemID} = Machine,
                            H = case data_homestead_seed:get({data_homestead_seed:get({seed_type,SeedItemID}),FriendLevel}) of
                                {Harvest,_Time,_Cost} ->
                                    Harvest;
                                _ ->
                                    0
                            end,
                            [Machine#p_homestead_machine{harvest = H}|AccList]
                        end, [], MachineList0),
				?unicast(RoleID,#sc_homestead_get_friend_info{roleID=FriendRoleID,baseinfo=HomesteadInfo2,machineList=MachineList});
		{false,ErrorCode}->
			?unicast(RoleID, #sc_homestead_error{reason_code=ErrorCode})
	end;
do_handle_info({client_msg, RoleID, #cs_homestead_get_friend_log{roleID=FriendRoleID}}) ->
	case check_operate_homestead(RoleID, FriendRoleID) of
		{true,_,_,_,_}->
				LogList = homestead_server:get_ets_homestead_logList(FriendRoleID),
				?unicast(RoleID,#sc_homestead_get_friend_log{roleID=FriendRoleID,list=LogList});
		{false,ErrorCode}->
			?unicast(RoleID, #sc_homestead_error{reason_code=ErrorCode})
	end;
%% 好友列表
do_handle_info({client_msg, RoleID, #cs_friend_get_list{type=Type}}) ->
	if Type =:= ?TYPE_PAL ->
		   #d_friend{pal=Pal} = get_friend(RoleID),
		   RoleEnargyFriendInfo =  enargy_server:get_ets_friend_enargy(RoleID),
		   PalInfoList = get_friend_detail(Pal,RoleEnargyFriendInfo),
		   ?unicast(RoleID, #sc_friend_get_list{roleInfoList=PalInfoList,type=Type,giveTimes=enargy_server:get_give_time(RoleEnargyFriendInfo)});
	   Type =:= ?TYPE_FOE->
		   #d_friend{foe=Foe} = get_friend(RoleID),
		   FoeInfoList = get_friend_detail(Foe),
		   ?unicast(RoleID, #sc_friend_get_list{roleInfoList=FoeInfoList,type=Type});
	   true ->
		   ?unicast(RoleID, #sc_friend_get_list{roleInfoList=[],type=Type})
	end;
do_handle_info({client_msg, RoleID, #cs_friend_more{roleIDList=RoleIDList}}) ->
	InfoList = get_friend_detail(RoleIDList),
	?unicast(RoleID, #sc_friend_more{roleInfoList=InfoList});

%% 获取可以添加好友的列表
do_handle_info({client_msg, RoleID, #cs_friend_get_add_list{}}) ->
	#d_friend{pal=Pal} = get_friend(RoleID),
	RoleIDList = [RoleID|Pal],  %% 这个列表中的id，不会出现在返回的addlist中
    RoleEnargyFriendInfo = enargy_server:get_ets_friend_enargy(RoleID),  
    #friend_enargy{addFriendList=AddFriendList} = RoleEnargyFriendInfo,
    Now = util:now(),
    AddFriendIDList =
        lists:foldr(
          fun({AddFriendID, S}, Acc) ->
                  case (Now-S) >= (3 * ?ONE_DAY_SECONDS) of
                      true ->
                          Acc;
                      false ->
                          [AddFriendID|Acc]
                  end
          end, [], AddFriendList),
    DelRoleIDList = RoleIDList ++ AddFriendIDList,
%% 	ServerRoleList = erlang:get(?random_pool),
%% 	#role{level=MyLevel} = db_sql:get_roleInfo(RoleID),
    MyLevel = role_lib:get_level(RoleID),
	ListByLevel = get_listByLevel(?ADDLIST_RANGE_INIT,MyLevel),
    ?INFO("L-cs_friend_get_add_list ListByLevel(level:~w,num:~w):~w",[MyLevel,length(ListByLevel),ListByLevel]),
    AdviseRoleIDList = random_add_list([RoleIDInList || {RoleIDInList,_} <- ListByLevel],DelRoleIDList),
    RoleInfoList =
        lists:foldr(
          fun(AdviseRoleID, Acc) ->
                  case role_lib:get_rolePublic(AdviseRoleID) of
                      [] ->
                          Acc;
                      RolePublic ->
                          StrangerInfo = rolePublic2p_stranger(RolePublic),
                          [StrangerInfo#p_stranger{canAdd=1}|Acc]
                  end  
          end, [], AdviseRoleIDList),
%% 	RoleInfoList = get_stranger_detail(RoleIDList, [], RoleEnargyFriendInfo),
	?unicast(RoleID, #sc_friend_get_add_list{roleList=RoleInfoList});

do_handle_info({client_msg, RoleID, #cs_friend_add{roleID=TarRoleID}}) ->
	#d_friend{pal=Pal} = get_friend(RoleID),
	case lists:member(TarRoleID, Pal) of
		true ->
			?unicast(RoleID,#sc_friend_add{result=2});
		false ->
			RoleEnargyFriendInfo = enargy_server:get_ets_friend_enargy(RoleID),
			case enargy_server:can_add_friend(RoleEnargyFriendInfo, TarRoleID) of
				1->
					RoleName = role_lib:get_name(RoleID),
					RoleLevel = role_lib:get_level(RoleID),
					mail_server:send_add_friend_mail(RoleID, RoleName,RoleLevel, TarRoleID),
					enargy_server:enargy_add_friend(RoleID, TarRoleID),
					?unicast(RoleID, #sc_friend_add{result=1});
				_->
					?unicast(RoleID,#sc_friend_add{result=5})
			end
	end;
do_handle_info({client_msg, RoleID, #cs_friend_explore{name=Name}}) ->
    DecodeList = util:gen_utf8_decode_list(erlang:list_to_binary(Name), []),
    case util:check_blankName(DecodeList) of
        true ->
            Length = util:calc_name_length(DecodeList),
            if Length > 24 orelse Length < 1 ->
                   RoleList = [];
               true ->
                   #d_friend{pal=FriendRoleIDList} = get_friend(RoleID),
                   RoleList = search_role(RoleID,FriendRoleIDList,Name)
            end;
        false ->
            RoleList = []
    end,
    ?unicast(RoleID, #sc_friend_explore{roleInfoList=RoleList});
do_handle_info({client_msg, RoleID, #cs_friend_delete{roleID=TarRoleID,type=Type}}) ->
	if Type =:= ?TYPE_PAL ->
		   del_friend(RoleID, TarRoleID),
		   del_friend(TarRoleID, RoleID),
		   db_sql:del_friend(RoleID, Type, TarRoleID),
		   _RoleName = role_lib:get_name(RoleID),
%% 		   mail_server:send_sys_mail(TarRoleID, ?MAIL_TEMPLATE_DEL_FRIEND, [RoleName], "", []),
            %删除私聊记录
           db_sql:del_whisper(RoleID,TarRoleID),
		   ?unicast(RoleID, #sc_friend_delete{roleID=TarRoleID,result=1,type=Type}),
		   ?unicast(TarRoleID, #sc_friend_notify_delete{roleID=RoleID,type=Type});
	   Type =:= ?TYPE_FOE->
		   #d_friend{foe=Foe} = FriendInfo = get_friend(RoleID),
		   Foe2 = lists:delete(TarRoleID, Foe),
		   FriendInfo2 = FriendInfo#d_friend{foe=Foe2},
		   db_sql:del_friend(RoleID, Type, TarRoleID),
		   set_friend(RoleID, FriendInfo2),
		   ?unicast(RoleID, #sc_friend_delete{roleID=TarRoleID,result=1,type=Type});
	   true ->
		   ?unicast(RoleID, #sc_friend_delete{roleID=TarRoleID,result=1,type=Type})
	end;
do_handle_info({add_friend, AgreeRoleID, RequestRoleID, MailUID}) ->
	enargy_server:enargy_remove_friend(RequestRoleID, AgreeRoleID),
	case check_add_friend(AgreeRoleID, RequestRoleID) of
		{true, AFriendInfo, RFriendInfo, ARole, RRole} ->
			AFriendInfo2 = AFriendInfo#d_friend{pal=[RequestRoleID|AFriendInfo#d_friend.pal]},
			RFriendInfo2 = RFriendInfo#d_friend{pal=[AgreeRoleID|RFriendInfo#d_friend.pal]},
			set_friend(AgreeRoleID, AFriendInfo2),
			set_friend(RequestRoleID,RFriendInfo2),
			db_sql:add_friend(AgreeRoleID, ?TYPE_PAL, RequestRoleID),
			?unicast(AgreeRoleID, #sc_mail_agree_friend{mailUID=MailUID,result=1}),
			?unicast(AgreeRoleID, #sc_friend_new{newFriend=RRole, type=?TYPE_PAL}),
			?unicast(RequestRoleID, #sc_friend_new{newFriend=ARole, type=?TYPE_PAL}),
			#rolePublic{isMale=IsMale1} = role_lib:get_rolePublic(AgreeRoleID),
			#rolePublic{isMale=IsMale2} = role_lib:get_rolePublic(RequestRoleID),
			IsDiffSex = IsMale1=/=IsMale2,
			role_task:send_dispach(AgreeRoleID, {dispach_task,role_add_friend,RequestRoleID,IsDiffSex}),
			case role_lib:is_online(RequestRoleID) of
				true->
					role_task:send_dispach(RequestRoleID, {dispach_task,role_add_friend,AgreeRoleID,IsDiffSex});
				false->
					role_task_trigger:offline_add_friend(RequestRoleID, AgreeRoleID,IsDiffSex)
			end;
		{false, Reason} ->
			?unicast(AgreeRoleID, #sc_mail_agree_friend{mailUID=MailUID,result=Reason})
	end;
do_handle_info({send_to_friend,RoleID,Record})->
	#d_friend{pal=List} = get_friend(RoleID),
	lists:foreach(fun(Rid)->
						  ?unicast(Rid,Record)
				  end, List);
do_handle_info({add_enemy, RoleID, EnemyID}) ->	
	do_add_enemy(RoleID, EnemyID);
do_handle_info({sync_homestead_ger,RoleID,GerTypeID,Qualiry})->
	#d_friend{pal=List} = get_friend(RoleID),
	Record = #sc_homestead_sync_ger{roleID=RoleID,gerTypeID=GerTypeID,gerQuality=Qualiry},
	lists:foreach(fun(Rid)->
						  ?unicast(Rid,Record)
				  end, List);
%% do_handle_info({cs_home_bounty_task_info,RoleID}) -> 
%%     #d_friend{pal=Pal} = get_friend(RoleID),
%%     erlang:send(home_server,{cs_home_bounty_task_info,RoleID,Pal});
do_handle_info({test_cs_home_bounty_task_info,RoleID}) -> 
    #d_friend{pal=Pal} = get_friend(RoleID),
    BountyTaskList = lists:foldr(fun(FriendId,AccList)-> 
                                        FTaskList = lists:filter(fun(Task) ->
                                                Task#home_task.status =:= 3
                                            end, home_server:get_home_task(FriendId)),
                                        FTaskList ++ AccList
                                 end, [], lists:usort([RoleID|Pal])), % 加入自己，剔除重复

    
    io:format("~n-----------dounty task start-------------~n"),
    lists:foreach(fun(T)->
            {TaskName} = data_home:get({home_task,T#home_task.quality,T#home_task.task_type}),
            Rate = if
                       T#home_task.status =:= 2 orelse T#home_task.status =:= 4 ->
                           role_home:cacl_win_rate(T);
                       true ->
                           0
                   end,
            io:format("~w(o:~w) (~w)~s q:~w s:~w(~w) a:~w enemy:~w need_time:~w ~w~n"
                     , [T#home_task.id
                       ,T#home_task.onwer_role_id
                       ,T#home_task.task_type,TaskName
                       ,T#home_task.quality
                       ,T#home_task.status,Rate
                       ,T#home_task.role_id
                       ,T#home_task.tgt_ger_type
                       ,T#home_task.finish_time / 3600
                       ,util:seconds_to_datetime(T#home_task.timtout_ts)])
        end, BountyTaskList),
    io:format("-----------dounty task end-----------~n");
do_handle_info(Info) ->
	throw({cannot_handle,Info}).





del_friend(RoleID, TarRoleID) ->
	#d_friend{pal=Pal} = FriendInfo = get_friend(RoleID),
	Pal2 = lists:delete(TarRoleID, Pal),
	FriendInfo2 = FriendInfo#d_friend{pal=Pal2},
	set_friend(RoleID, FriendInfo2).

random_add_list(RoleIDList,DelRoleIDList)->
	random_add_list(RoleIDList,DelRoleIDList,[],?ADVISE_ROLE_NUM,5).

random_add_list(_,_,Acc,0,_)->
	Acc;
random_add_list(_,_,Acc,_,0)->
	Acc;
random_add_list(RoleIDList,DelRoleIDList,Acc,Num,Times)->
	List1 = util:random_list(RoleIDList, Num)--Acc,
	List = lists:filter(fun(ID)->
								not lists:member(ID, DelRoleIDList) andalso length((get_friend(ID))#d_friend.pal)<get_max_friend_num(ID)
				 end, List1),
	random_add_list(RoleIDList,DelRoleIDList,Acc++List,Num-length(List),Times-1).

do_add_enemy(RoleID, EnemyID) ->
	#d_friend{foe=Foe} = AFriendInfo = get_friend(RoleID),
	case lists:any(fun(E) -> E=:=EnemyID end, Foe) of
		false ->
            case erlang:length(Foe) >= ?MAX_FRIEND_NUM of
                true ->
                    {DelEnemyIDList, LeftEnemyIDList} =
                        lists:foldr(fun(_, {AccDelIDList, [DelID|LeftIDList]}) ->
                                        {[DelID|AccDelIDList], LeftIDList}
                                    end, {[], Foe}, lists:seq(1, erlang:length(Foe) - ?MAX_FRIEND_NUM + 1)),
                    lists:foreach(fun(DelEnemyID) ->
                                    db_sql:del_friend(RoleID, ?TYPE_FOE, DelEnemyID)
                                  end, DelEnemyIDList),
                    NewFoe = lists:append(LeftEnemyIDList, [EnemyID]);
                false ->
                    NewFoe = lists:append(Foe, [EnemyID])
            end,
            AFriendInfo2 = AFriendInfo#d_friend{foe=NewFoe},
            set_friend(RoleID, AFriendInfo2),
            db_sql:add_friend(RoleID, ?TYPE_FOE, EnemyID);
		true ->
			set_friend(RoleID, AFriendInfo)
	end.
			



%% 检查增加好友
check_add_friend(AgreeRoleID,RequestRoleID) ->
	#d_friend{pal=APal} = AFriendInfo = get_friend(AgreeRoleID),
	Max1 = get_max_friend_num(AgreeRoleID),
	if length(APal) >= Max1  ->
		   {false, 3};
	   true ->			
		   #d_friend{pal=RPal} = RFriendInfo = get_friend(RequestRoleID),
		   Max2 = get_max_friend_num(RequestRoleID),
		   if length(RPal) >= Max2 ->
				  {false, 2};
			  true ->
				  case lists:member(AgreeRoleID, RPal) orelse lists:member(RequestRoleID, APal) of
					  true ->
						  {false, 5};
					  false ->
						  FRoleEnargyFriendInfo = enargy_server:get_ets_friend_enargy(RequestRoleID),
						  case get_friend_detail2(AgreeRoleID,FRoleEnargyFriendInfo) of
							  [] ->
								  {false, 6};
							  ARole ->
								  RoleEnargyFriendInfo = enargy_server:get_ets_friend_enargy(AgreeRoleID),
								  case get_friend_detail2(RequestRoleID,RoleEnargyFriendInfo) of
									  [] ->
										  {false, 7};
									  RRole ->
										  {true, AFriendInfo, RFriendInfo, ARole, RRole}
								  end
						  end
				  end
		   end
	end.

search_role(RoleID,FriendRoleIDList,Name) ->
	case db_sql:search_fuzzy_roleName(Name) of
		?undefined ->
			[];
		AddRoleIDList ->
			RoleEnargyFriendInfo = enargy_server:get_ets_friend_enargy(RoleID),
			get_stranger_detail(lists:delete(RoleID, AddRoleIDList), FriendRoleIDList, RoleEnargyFriendInfo)
	end.

get_friend(RoleID) ->
	case get(RoleID) of
		#d_friend{}=Info ->
			next;
		?undefined ->
			Info = get_db_friend(RoleID) 
	end,
    #d_friend{pal=PalList} = Info,
    Info#d_friend{pal=lists:filter(fun(E) -> E =/= RoleID end, PalList)}.

get_db_friend(RoleID) ->
	case db_sql:get_friendList(RoleID) of
		#d_friend{}=Info ->
			Info;
		_ ->
			#d_friend{foe=[],pal=[],roleID=RoleID}
	end.

set_friend(RoleID, FriendInfo) ->
	erlang:put(RoleID, FriendInfo).

get_friend_detail(RoleIDList)->
	get_friend_detail(RoleIDList,undefined).

get_friend_detail(RoleIDList,RoleEnargyFriendInfo) ->
	get_friend_detail(RoleIDList, [],RoleEnargyFriendInfo).

get_friend_detail([RoleID|RoleIDList],Result,RoleEnargyFriendInfo) ->
	case get_friend_detail2(RoleID,RoleEnargyFriendInfo) of
		[] ->
			get_friend_detail(RoleIDList, Result,RoleEnargyFriendInfo);
		Friend ->
			get_friend_detail(RoleIDList, [Friend|Result],RoleEnargyFriendInfo)
	end;
get_friend_detail([], Result,_RoleEnargyFriendInfo) ->
	Result.

get_friend_detail2(RoleID,RoleEnargyFriendInfo) ->
	case role_lib:get_rolePublic(RoleID) of
		[] ->
			[];
		RolePublic ->
			case RoleEnargyFriendInfo of
				undefined->
					rolePublic2p_friend(RolePublic);
				_->
					#rolePublic{level=Level,fightPower=FightPower} = RolePublic,
					CanGive = enargy_server:can_give_friend_enargy(RoleEnargyFriendInfo, RoleID),
					CanSend = enargy_server:can_send_friend_enargy(RoleEnargyFriendInfo, RoleID),
					SendS = enargy_server:get_send_second(RoleEnargyFriendInfo, RoleID),
					FriendInfo = rolePublic2p_friend(RolePublic),
					{MatingCoolSecond,GerTypeID,GerQuality,BeginGold,EndGold,BeginBadge,EndBadge} = homestead_server:get_ets_homestead_friend_info(RoleID,Level),
					FriendInfo#p_friend{matingCoolSecond=MatingCoolSecond,gerTypeID=GerTypeID,gerQuality=GerQuality,beginGold=BeginGold,endGold=EndGold,
										beginBadge=BeginBadge,endBadge=EndBadge,canGive=CanGive,canSend=CanSend,sendS=SendS,fightPower=FightPower}
			end
	end.

rolePublic2p_friend(RolePublic=#rolePublic{roleID=RoleID}) ->
	LastLogoutTime = case role_lib:is_online(RoleID) of
					   true ->
						   0;
					   _ ->
						   RolePublic#rolePublic.lastLogoutTime
                     end,
    #p_friend{
              title=RolePublic#rolePublic.title,
              roleName=RolePublic#rolePublic.roleName,
              roleID=RoleID,
              logoutTime=LastLogoutTime,
              level=RolePublic#rolePublic.level,
              isMale=RolePublic#rolePublic.isMale,
              fightPower=RolePublic#rolePublic.fightPower,
              location=RolePublic#rolePublic.location,
              head=RolePublic#rolePublic.head,
             chapterID=RolePublic#rolePublic.xbattleChapterID}.

rolePublic2p_stranger(RolePublic) ->
	#p_stranger{
			  title=RolePublic#rolePublic.title,
			  roleName=RolePublic#rolePublic.roleName,
			  roleID=RolePublic#rolePublic.roleID,
			  logoutTime=RolePublic#rolePublic.lastLogoutTime,
			  level=RolePublic#rolePublic.level,
			  isMale=RolePublic#rolePublic.isMale,
			  fightPower=RolePublic#rolePublic.fightPower,
			  location=RolePublic#rolePublic.location,
			  head=RolePublic#rolePublic.head}.
	
%%得到陌生人					 
get_stranger_detail(AddRoleIDList,FriendRoleIDList,RoleEnargyFriendInfo)->
	get_stranger_detail(AddRoleIDList,FriendRoleIDList,RoleEnargyFriendInfo,[]).

get_stranger_detail([],_,_,Acc)->
	Acc;
get_stranger_detail([RoleID|TailRoleIDList],FriendRoleIDList,RoleEnargyFriendInfo,Acc)->
	case role_lib:get_rolePublic(RoleID) of
		[] ->
			get_stranger_detail(TailRoleIDList,FriendRoleIDList,Acc);
		RolePublic ->
			StrangerInfo = rolePublic2p_stranger(RolePublic),
			case lists:member(RoleID, FriendRoleIDList) of
				true->
					get_stranger_detail(TailRoleIDList,FriendRoleIDList,RoleEnargyFriendInfo,[StrangerInfo#p_stranger{canAdd=2}|Acc]);
				false->
					CanAdd =  enargy_server:can_add_friend(RoleEnargyFriendInfo,RoleID),
					get_stranger_detail(TailRoleIDList,FriendRoleIDList,RoleEnargyFriendInfo,[StrangerInfo#p_stranger{canAdd=CanAdd}|Acc])
			end
	end.

clear_buff() ->
	lists:foreach(fun({RoleID,#d_friend{}}) when is_integer(RoleID) ->
						  erlang:erase(RoleID);
%% 					 ({?random_pool,_Pool}) ->
%% 						  Var = random:uniform(4),
%% 						  if Var =:= 1 ->
%% 								 erlang:erase(?random_pool);
%% 							 true ->
%% 								 ignore
%% 						  end;
					 (_) ->
						  ignore				  
				  end, erlang:get()).

%% 检查本进程buff的tick
tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).
tick_refresh() ->
    erlang:send_after(?TICK_INTERVAL_REFRESH*1000, self(), tick_refresh).


get_max_friend_num(RoleID)->
	#rolePublic{viplevel=VipLevel} = role_lib:get_rolePublic(RoleID),
	case data_vip:get(VipLevel) of
		?undefined->
			0;
		#data_vip{max_friend_num=MaxFriendNum}->
			MaxFriendNum
	end.

%% 根据等级范围，取出符合条件的子集
get_listByLevel(LevelRange,MyLevel)->
    MaxLevel = MyLevel + LevelRange,
    MinLevel = MyLevel - LevelRange,
    RobotId = tk_id:robot_roleID_max(),
    RetFromEts = ets:select(?ETS_ROLE_LEVEL, [{{'$1','$2'},
                                              [{'andalso',{'>=',MaxLevel,'$2'},{'andalso',{'>=','$2',MinLevel},{'>=','$1',RobotId}}}],
                                              [{{'$1','$2'}}]}]),
    if
        length(RetFromEts) >= ?ADDLIST_RANGE_TOTAL ->
            lists:sublist(RetFromEts, ?ADDLIST_RANGE_TOTAL);
        LevelRange =< 40 ->
            get_listByLevel(LevelRange+5, MyLevel);
        true -> %虽然list数量不足，但是level范围已经够大了
            RetFromEts
    end.
	
get_role_friend_roleID(RoleID)->
	gen_server:call(?MODULE, {get_role_friend_roleID,?TYPE_PAL,RoleID}).

get_role_enemy_roleID(RoleID)->
	gen_server:call(?MODULE, {get_role_friend_roleID,?TYPE_FOE,RoleID}).