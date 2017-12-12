%% @author zcl
%% @doc 限时打折
%% Created 2014-01-08
-module(discount_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================

-define(dump_interval, 300).			% 保存数据间隔
-define(check_interval, 300).		% 检查状态改变间隔

-define(status, status).			% 服务器当前开启的活动 activity，保存当前执行的活动配置信息序号，该序号在配置文档中可以查到
-define(fireInfo, fireInfo).		% 服务器信息
-define(roleInfo, roleInfo).		% 玩家信息
-define(pushList, pushList).		% 推送列表，保存需要得到推送消息的玩家
-define(ETS_DISCOUNT_ACTIVITY_LIST,ets_discount_activity_list).     %保存限时打折活动列表
-define(pay_reward_role_list,pay_reward_role_list).
-record(need_mail_pay_reward_role_list,{activityID=0,activityrank=0,rolelist=[]}).

%% ===================Dict Key End   =========================


%% =========================================================================================


%% 服务器信息
init_discount_info() ->
	DiscountInfo =
		case db_sql:get_etc(?DB_ETC_KEY_DISCOUNT) of
			X when is_record(X,d_discount_info)->
				X;
			_ ->
				#d_discount_info{}
		end,
	NewDiscountInfo = add_activity_from_config(DiscountInfo),
	?INFO("NewDiscountInfo: ~w ~n ",[NewDiscountInfo]),
	set_discount_info(NewDiscountInfo),

	%%从数据库中加载能够获得充值奖励的玩家信息列表
	Can_get_pay_reward_list = 
		case db_sql:get_etc(?DB_ETC_KEY_CAN_GET_PAY_ROLELIST) of
			Y when is_list(Y)->
				Y;
			_ ->
				[]
		end,
	set_can_get_pay_reward_role_list(Can_get_pay_reward_list).

get_discount_info(Key)->
	ets:lookup(?ETS_DISCOUNT_ACTIVITY_LIST,Key).
set_discount_info(DiscountInfo) ->
	ets:delete_all_objects(?ETS_DISCOUNT_ACTIVITY_LIST),
	lists:foreach(fun(ActivityUnit)->
		% ?INFO("ActivityUnit: ~w ~n ",[ActivityUnit]),
		ets:insert(?ETS_DISCOUNT_ACTIVITY_LIST,ActivityUnit)
	end,DiscountInfo#d_discount_info.activitylist).
	
persist_discount_info() ->
	ActivityList = ets:foldl(fun(Element,Acc)->
		[Element|Acc]
		end,[],?ETS_DISCOUNT_ACTIVITY_LIST),
	DiscountInfo = #d_discount_info{activitylist=ActivityList},
	?INFO("准备持久化的Discount_Info:~w ~n ",[DiscountInfo]),
	db_sql:set_etc(?DB_ETC_KEY_DISCOUNT, DiscountInfo),
	Can_get_pay_reward_list = get_can_get_pay_reward_role_list(),
	db_sql:set_etc(?DB_ETC_KEY_CAN_GET_PAY_ROLELIST,Can_get_pay_reward_list).

get_discount_info()->
	ActivityList = ets:foldl(fun(Element,Acc)->
		[Element|Acc]
	end,[],?ETS_DISCOUNT_ACTIVITY_LIST),
	#d_discount_info{activitylist=ActivityList}.

set_can_get_pay_reward_role_list(PayRewardRoleList)->
	put(?pay_reward_role_list,PayRewardRoleList).

get_can_get_pay_reward_role_list()->
	case get(?pay_reward_role_list) of
		?undefined->
			[];
		Other->
			Other
	end.

%% ===================================================================


start() ->
	{ok,_}=supervisor:start_child(world_sup, 
								  {?MODULE,
								   {?MODULE, start_link, []},
								   permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	ets:new(?ETS_DISCOUNT_ACTIVITY_LIST, [{keypos, 2}, set, public, named_table]),

	init_discount_info(),
	check_tick(),
	dump_tick(),
	erlang:send_after(?dump_interval, self(), do_hibernate),
	{ok, get_discount_info()}.

handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info(do_hibernate,State)->
	erlang:send_after(?dump_interval, self(),do_hibernate),
	erlang:garbage_collect(),
	{noreply, State,hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};

handle_info(Info, State) ->
	?CATCH(do_handle_info(State, Info)),
	{noreply, State}.

terminate(Reason, State) ->
	persist_discount_info(),
	% persist_role_info(),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% %% ====================================================================

do_handle_info(_State, check_tick)->
	check_tick(),
	update_discount_info();

do_handle_info(_State, dump_tick)->
	dump_tick(),
	persist_discount_info();

do_handle_info(_State,reload_activity_config)->
	reload_activity_config();

do_handle_info(_State,remove_all_activity)->
	remove_all_activity();

%%保存某一个活动能够领奖的玩家列表
%-record(pay_activity_can_get_list,{activityID=0,rolelist=[]}).
%%保存玩家满足限时打折充值活动的形象
%-record(satisfied_discount_pay_info,{activityID=0,ranklist=[]}).
%%保存玩家能够领取的活动
%-record(pay_activity_can_get_unit,{roleID=0,activityList = []}).
do_handle_info(_State,{addrole,RoleID,ActivityUnit})->
	?INFO("discount_server receive addroleinfo roleID:~w Activity:~w ~n",[RoleID,ActivityUnit]),
	Can_get_pay_reward_list = get_can_get_pay_reward_role_list(),
	Can_get_pay_reward_list1 = case lists:keytake(ActivityUnit#satisfied_discount_pay_info.activityID,#pay_activity_can_get_list.activityID,Can_get_pay_reward_list) of
		{_,FindList,Other}->
			case lists:keytake(RoleID,#pay_activity_can_get_unit.roleID,FindList#pay_activity_can_get_list.rolelist) of
				{_,FindUnit,OtherUnit}->
					NewUnit = update_can_get_pay_unit(FindUnit,ActivityUnit),
					NewRoleInfoList = [FindUnit#pay_activity_can_get_unit{activityList=NewUnit}|OtherUnit];
				false ->
					NewRoleInfoList = [#pay_activity_can_get_unit{roleID=RoleID,activityList=[ActivityUnit]}|FindList#pay_activity_can_get_list.rolelist]
			end,
			NewFindList = FindList#pay_activity_can_get_list{rolelist=NewRoleInfoList},
			[NewFindList|Other];
		false ->
			NewRoleInfoList = [#pay_activity_can_get_unit{roleID=RoleID,activityList=[ActivityUnit]}],
			[#pay_activity_can_get_list{activityID=ActivityUnit#satisfied_discount_pay_info.activityID,rolelist=NewRoleInfoList}|Can_get_pay_reward_list]
	end,
	set_can_get_pay_reward_role_list(Can_get_pay_reward_list1);

do_handle_info(_State,{deleterole,RoleID,ID,RealActivityID})->
	?INFO("discount_server receive deleteroleinfo RoleID:~w ID:~w RealActivityID:~w ~n ",[RoleID,ID,RealActivityID]),
	Can_get_pay_reward_list = get_can_get_pay_reward_role_list(),
	Can_get_pay_reward_list1 = case lists:keytake(RealActivityID,#pay_activity_can_get_list.activityID,Can_get_pay_reward_list) of
		{_,FindList,Other}->
			NewFindList = delete_can_get_pay_unit(FindList,RoleID,ID,RealActivityID),
			[NewFindList|Other];
		false ->
			?INFO("删除可领奖的活动居然不存在：RoleID:~w ActivityRankID:~w RealActivityID:~w ExistList:~w ~n",[RoleID,ID,RealActivityID,Can_get_pay_reward_list]),
			Can_get_pay_reward_list
	end,
	set_can_get_pay_reward_role_list(Can_get_pay_reward_list1);

do_handle_info(_State,clear_can_get_list)->
	do_clear_can_get_list();

do_handle_info(State, Info) ->
	?ERR("do_handle_info function clause:request=~100p,state=~100p",[Info, State]).

check_tick()->
	erlang:send_after(?check_interval * 1000, self(), check_tick).

dump_tick()->
	erlang:send_after(?dump_interval * 1000, self(), dump_tick).

%%加载配置文件中的活动到当前活动列表中
add_activity_from_config(OldActivity)->
	ActivityList = data_discount:get(data_activity_list),
	LastActivityList = lists:foldl(fun(Activity,Acc)->
		{ActivityID,ActivityConditionList,BeginTime,EndTime,RealActivityID} = Activity,
		case lists:keyfind(ActivityID,#d_discount_activity_unit.activityID,Acc) of
			false ->
				BeginTimeStamp = util:datetime_to_seconds(BeginTime),
				EndTimeStamp = util:datetime_to_seconds(EndTime),
				CurrentStamp = util:now(),
				% ?INFO("BeginTimeStamp:~w EndTimeStamp:~w CurrentStamp:~w ~n",[BeginTimeStamp,EndTimeStamp,CurrentStamp]),
				case  CurrentStamp =< EndTimeStamp of 
					true -> 
						[#d_discount_activity_unit{activityID=ActivityID,activity_condition=ActivityConditionList,real_activityID=RealActivityID,begintime=BeginTimeStamp,endtime=EndTimeStamp}|Acc];
					false ->
						Acc
				end;
			_ ->
				Acc
		end
	end,OldActivity#d_discount_info.activitylist,ActivityList),
	OldActivity#d_discount_info{activitylist=LastActivityList}.

%%更新ets表中记录的打折活动信息
update_discount_info()->
	{ActivityList,ResultList} = ets:foldl(fun(Element,{Acc,Result})->
		case is_record(Element,d_discount_activity_unit) of
			true ->
				CurrentStamp = util:now(),
				case CurrentStamp =< Element#d_discount_activity_unit.endtime of
					true ->
						{[Element|Acc],Result};
					false ->
						send_unaccept_pay_reward(Element),
						{Acc,[false|Result]}
				end;
			false ->
				?INFO("ets表中发现不属于d_discount_activity_unit的活动"),
				Acc
		end
		end,{[],[]},?ETS_DISCOUNT_ACTIVITY_LIST),
	NewDiscountInfo = #d_discount_info{activitylist=ActivityList},
	set_discount_info(NewDiscountInfo),
	case length(ResultList) =:=0 of
		true->
			ignore;
		false ->
			do_bc_msg_except(update_discount_info,[])
	end.

reload_activity_config() ->
	DiscountInfo = #d_discount_info{},
	NewDiscountInfo = add_activity_from_config(DiscountInfo),
	?INFO("NewDiscountInfo: ~w ~n ",[NewDiscountInfo]),
	set_discount_info(NewDiscountInfo),
	do_bc_msg_except(update_discount_info,[]).

do_bc_msg_except(Msg, ExceptRoleIDList) ->
	
	RoleIDList = [E||{E,_}<-ets:tab2list(?ETS_ROLE_ONLINE)],
    lists:foreach(fun(RoleID) ->
                          case lists:member(RoleID, ExceptRoleIDList) of
                              false ->
                              	  spawn(fun()-> catch role_lib:send_server(RoleID,Msg) end);
                              true ->
                                  next
                          end
                  end, RoleIDList).

remove_all_activity()->
	ets:foldl(fun(Element,_Acc)->
		send_unaccept_pay_reward(Element)
	end,0,?ETS_DISCOUNT_ACTIVITY_LIST),
	ets:delete_all_objects(?ETS_DISCOUNT_ACTIVITY_LIST),
	do_bc_msg_except(update_discount_info,[]).

%%-record(satisfied_discount_pay_info,{activityID=0,ranklist=[]}).
%%Activity:{satisfied_discount_pay_info,101,[10102,10101]}
%%将玩家进程发送来的充值活动领取更新到旧的领取数据中 OldUnit作为pay_activity_can_get_unit列表，ActivitUnit为satisfied_discount_pay_info类型tuple
update_can_get_pay_unit(OldUnit,ActivityUnit)->
	?INFO("OldUnit:~w ActivityUnit~w ~n ",[OldUnit,ActivityUnit]),
	case lists:keytake(ActivityUnit#satisfied_discount_pay_info.activityID,#satisfied_discount_pay_info.activityID,OldUnit#pay_activity_can_get_unit.activityList) of
		{_,FindUnit,Other}->
			NewRankList = 
				lists:foldl(fun(PayConfigID,Acc)->
					case lists:member(PayConfigID,Acc) of
						false ->
							[PayConfigID|Acc];
						_ ->
							Acc
					end
				end,FindUnit#satisfied_discount_pay_info.ranklist,ActivityUnit#satisfied_discount_pay_info.ranklist),
			[FindUnit#satisfied_discount_pay_info{ranklist=NewRankList}|Other];
		false ->
			[ActivityUnit|OldUnit]
	end.

% #pay_activity_can_get_list{activityID = 101,
%                             rolelist = [#pay_activity_can_get_unit{roleID = 31010016,
%                                                                    activityList = [#satisfied_discount_pay_info{activityID = 101,
%                                                                                                                 ranklist = [10101]}]},
%                                         #pay_activity_can_get_unit{roleID = 31010015,
%                                                                    activityList = [#satisfied_discount_pay_info{activityID = 101,
%                                                                                                                 ranklist = [10102,10101]}]}]},

%%将FindList列表中保存的RoleID玩家对应的ID活动记录删除
delete_can_get_pay_unit(FindList,RoleID,ID,RealActivityID)->
	?INFO("FindList:~w RoleID:~w ID:~w ~n",[FindList,RoleID,ID]),
	case lists:keytake(RoleID,#pay_activity_can_get_unit.roleID,FindList#pay_activity_can_get_list.rolelist) of
		{_,FindRoleInfo,Other}->
			?INFO("FindRoleInfo:~w ~n",[FindRoleInfo]),
			SatisfiedList = FindRoleInfo#pay_activity_can_get_unit.activityList,
			case lists:keytake(RealActivityID,#satisfied_discount_pay_info.activityID,SatisfiedList) of
				{_,FindSatisfiedUnit,OtherSatisfiedUnit}->
					RankList = FindSatisfiedUnit#satisfied_discount_pay_info.ranklist -- [ID],
					case length(RankList) =/=0 of
						true ->
							NewFindSatisfiedUnit = FindSatisfiedUnit#satisfied_discount_pay_info{ranklist=RankList},
							NewSatisfiedList = [NewFindSatisfiedUnit|OtherSatisfiedUnit],
							NewFindRoleInfo = FindRoleInfo#pay_activity_can_get_unit{activityList = NewSatisfiedList},
							NewRoleList = [NewFindRoleInfo|Other],
							FindList#pay_activity_can_get_list{rolelist=NewRoleList};
						false ->
							NewFindRoleInfo = FindRoleInfo#pay_activity_can_get_unit{activityList =OtherSatisfiedUnit},
							NewRoleList = case OtherSatisfiedUnit =:= [] of
								true->
									Other;
								false->
									[NewFindRoleInfo|Other]
							end,
							FindList#pay_activity_can_get_list{rolelist=NewRoleList}
					end;
				false ->
					?INFO("删除的活动号不在记录中：FindList ~w RoleID:~w ID:~w ~n",[FindList,RoleID,ID]),
					FindList
			end;
		false ->
			?INFO("删除可领奖的活动 玩家居然不存在：RoleID:~w ActivityRankID:~w ExistList:~w ~n",[RoleID,ID,FindList]),
			FindList
	end.


%-record(d_discount_activity_unit,{
%	activityID=0,activity_condition=[],real_activityID=0,begintime=0,endtime=0
%	}).
%%所有没有及时领取充值兑换奖励的玩家发送奖励邮件,Element为d_discount_activity_unit结构
%[{need_mail_pay_reward_role_list,101,10101,[31010016]},
 %{need_mail_pay_reward_role_list,101,10102,[31010016]}]

send_unaccept_pay_reward(Element)->
	Can_get_pay_reward_list = get_can_get_pay_reward_role_list(),
	case lists:keytake(Element#d_discount_activity_unit.real_activityID,#pay_activity_can_get_list.activityID,Can_get_pay_reward_list) of
		false ->
			?INFO("没有发现需要发送充值兑换奖励的数据：Can_get_pay_reward_list:~w  即将删除的活动：~w ~n ",[Can_get_pay_reward_list,Element]);
		{_,FindList,Other} ->
			Result = get_all_need_mail_pay_reward_role_list(FindList),
			?INFO("返回的需要邮件发奖的列表：~w~n",[Result]),
			send_mail_pay_reward(Result),
			set_can_get_pay_reward_role_list(Other)
	end.
%-record(need_mail_pay_reward_role_list,{activityID=0,activityrank=0,rolelist=[]}).
%%从pay_activity_can_get_list中获得所有玩家的roleID,返回need_mail_pay_reward_role_list结构
get_all_need_mail_pay_reward_role_list(CanGetList)->
	% ActivityContent = data_discount:get({data_discount_activity,CanGetList#pay_activity_can_get_list.activityID}),
 %    {_,List} = ActivityContent,
 	case data_discount:get({data_discount_activity,CanGetList#pay_activity_can_get_list.activityID}) of
 		?undefined->
 			[];
 		{_,List}->
    		%%首先构造需要返回的整体结构体
    		Result = [#need_mail_pay_reward_role_list{activityID=CanGetList#pay_activity_can_get_list.activityID,activityrank=Id}||{Id,Need,Reward,Times}<-List],
    		?INFO("初始构造出来的返回结构体：~w ~n",[Result]),
    		lists:foldl(fun(CanGetUnit,Acc)->
    			case lists:keyfind(CanGetList#pay_activity_can_get_list.activityID,#satisfied_discount_pay_info.activityID,CanGetUnit#pay_activity_can_get_unit.activityList) of
    				false ->
    					?INFO("没有发现活动对应的玩家列表：~w ~n",[CanGetList]),
    					Acc;
    				FindOne ->
    					lists:foldl(fun(ActivityRankID,Acc0)->
    						case lists:keytake(ActivityRankID,#need_mail_pay_reward_role_list.activityrank,Acc0) of
    							{_,FindNeedList,OtherNeedList}->
    						 		[FindNeedList#need_mail_pay_reward_role_list{rolelist=[CanGetUnit#pay_activity_can_get_unit.roleID]++FindNeedList#need_mail_pay_reward_role_list.rolelist}|OtherNeedList];
    							false ->
    								?INFO("出现错误：玩家的rankID：~w 不在构造的列表中：~w ~n",[ActivityRankID,Acc0]),
    								Acc0
    						end
    					end,Acc,FindOne#satisfied_discount_pay_info.ranklist)
    			end
    		end,Result,CanGetList#pay_activity_can_get_list.rolelist)
    end.

%%向玩家发送邮件奖励
%NeedMailList为-record(need_mail_pay_reward_role_list,{activityID=0,activityrank=0,rolelist=[]})结构
%[{need_mail_pay_reward_role_list,101,10101,[31010016]},
%{need_mail_pay_reward_role_list,101,10102,[31010016]}]
send_mail_pay_reward(NeedMailList)->
	lists:foreach(fun(Element)->
		PayActivityContent = data_discount:get({data_discount_activity,Element#need_mail_pay_reward_role_list.activityID}),
        {_,PayActivityConfigList} = PayActivityContent,
        PayConfigList = [#p_pay_config_unit{id=ConfigID,payrank=NeedConfig,getconfig=role_discount:transfer2rewardunit(RewardConfig),times=0,totaltimes=TotalTimes}||{ConfigID,NeedConfig,RewardConfig,TotalTimes}<-PayActivityConfigList],
        case lists:keyfind(Element#need_mail_pay_reward_role_list.activityrank,#p_pay_config_unit.id,PayConfigList) of
        	false ->
        		?INFO("没有找到发送邮件奖励列表中的活动配置 NeedMailList:~w ID: ~w ~n",[NeedMailList,Element#need_mail_pay_reward_role_list.activityrank]);
        	FindOne ->
        		Reward = role_discount:transfer2rewardlist(FindOne),
        		% mail_server:send_sys_mail(RoleID, 1091, [], "", Reward)
        		role_mail_gift:send_gift(Element#need_mail_pay_reward_role_list.rolelist,Reward, "", ?MAIL_DISCOUNT_PAY_ACTIVITY_REWARD, "")
        		% role_mail_gift:send_gift_c(Element#need_mail_pay_reward_role_list.rolelist,Reward,"test")
        end
	end,NeedMailList).
	
do_clear_can_get_list()->
	CanGetListUnitList = get_can_get_pay_reward_role_list(),
	NewCanGetListUnitList = lists:foldl(fun(CanGetListUnit,Acc)->
		case is_record(CanGetListUnit,pay_activity_can_get_list) of
			true->
				#pay_activity_can_get_list{activityID=ActivityID,rolelist=CanGetRoleUnitList} = CanGetListUnit,
					case data_discount:get({data_discount_activity,ActivityID}) of
						?undefined->
							Acc;
						_ ->
							NewRoleList = lists:foldl(fun(CanGetUnit,Acc)->
								#pay_activity_can_get_unit{activityList=ActivityList} = CanGetUnit,
								case ActivityList=:=[] of
									true->
										Acc;
									false->
										[CanGetUnit|Acc]
								end
							end,[],CanGetRoleUnitList),
							[CanGetListUnit#pay_activity_can_get_list{rolelist=NewRoleList}|Acc]
					end;
			false->
				Acc
		end
	end,[],CanGetListUnitList),
	set_can_get_pay_reward_role_list(NewCanGetListUnitList).
%%test---------------------------------------------------------------------------	
test_reload_activity_config()->
	erlang:send(?MODULE,reload_activity_config).

test_remove_all_activity()->
	erlang:send(?MODULE,remove_all_activity).

test_clear_can_get_list()->
	erlang:send(?MODULE,clear_can_get_list).