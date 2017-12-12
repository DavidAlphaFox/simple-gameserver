%% @author zcl
%% @doc 砸金蛋
%% Created 2016-3-16
-module(goldenegg_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").


-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(dump_interval,300*1000).
-define(DYNAMIC,dynamic).
-define(SOLID,solid).
%% ===================ETS Key Begin==========================
-define(ETS_ROLE_GOLDENEGG_INFO_LIST,ets_role_goldenegg_info_list). 
%% ====================ETS Key end===========================

%% ===================Dict Key Begin =========================
-define(STATE_SMASH,1).
-define(STATE_EXCHANGE,2).
-define(STATE_CLOSE,3).

%% ===================Dict Key End   =========================

%% ===================record define begin=====================
-record(state,{next_refresh_time=0,goodslist=[],begintime=0,smashendtime=0,exchangeendtime=0,state=0,next_change_status_timestamp=0,change_status_time_ref=0}).
%% ===================record define end=======================



%% =========================================================================================

%%初始化砸金蛋服务进程
init_goldenegg_server()->
	X = db_sql:get_etc(?DB_ETC_KEY_GOLDENEGG),
	case X of
		{State,RoleEggInfoList} when is_record(State,state) ->
			NewState = init_reset_info(State),
			init_roleegginfo(RoleEggInfoList,NewState),
			NewState;
		_ ->
			init_goldenegg_server2()
	end.
init_goldenegg_server2()->
	GoodsList = get_next_goodsIDList(),
	{BeginDateTime,SmashEndDateTime} = data_goldenegg:get(opentime),
	ExchangeInterval = data_goldenegg:get(exchange_interval),
	BeginTimeStamp = util:datetime_to_seconds(BeginDateTime),
	SmashEndTimeStamp = util:datetime_to_seconds(SmashEndDateTime),
	ExchangeEndTimeStamp = SmashEndTimeStamp + ExchangeInterval,
	State = get_current_state(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
	NextFreshTimeStamp = get_next_fresh_timestamp(),
	NextChangeStatusTimeStamp = get_current_next_change_status_timestamp(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
	#state{next_refresh_time=NextFreshTimeStamp,goodslist=GoodsList,begintime=BeginTimeStamp,smashendtime=SmashEndTimeStamp,exchangeendtime=ExchangeEndTimeStamp,state=State,next_change_status_timestamp=NextChangeStatusTimeStamp}.

init_roleegginfo(RoleEggInfoList,NewState)->
	lists:foreach(fun({_Key,EggInfo})->
		case is_record(EggInfo,role_egg_info) of
			true->
				case NewState#state.state of
					?STATE_CLOSE->
						ignore;
					_->
						NowSec = util:now(),
						case EggInfo#role_egg_info.validtime >= NowSec of
							true->
								ets:insert(?ETS_ROLE_GOLDENEGG_INFO_LIST,{EggInfo#role_egg_info.roleID,EggInfo});
							false->
								ets:insert(?ETS_ROLE_GOLDENEGG_INFO_LIST,{EggInfo#role_egg_info.roleID,EggInfo#role_egg_info{score=0}})
						end
				end;
			false->
				?ERR("undefined role_egg_info:~w ~n",[EggInfo]),
				ignore
		end
	end,RoleEggInfoList).

% -record(state,{next_refresh_time=0,goodslist=[],begintime=0,smashendtime=0,exchangeendtime=0,state=0,next_change_status_timestamp=0}).
init_reset_info(State)->
	#state{begintime=BeginTimeStamp,smashendtime=SmashEndTimeStamp,exchangeendtime=ExchangeEndTimeStamp} = State,
	NowSec = util:now(),
	case NowSec > ExchangeEndTimeStamp of
		true-> %%上次的数据时间完全过时，根据配置重新初始化一个新的
			init_goldenegg_server2();
		false->
			NewNextFreshTime = get_next_fresh_timestamp(),
			NewState = get_current_state(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
			NewNextChangeStatusTimeStamp = get_current_next_change_status_timestamp(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
			State#state{next_refresh_time=NewNextFreshTime,state=NewState,next_change_status_timestamp=NewNextChangeStatusTimeStamp}
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
	NowSec = util:now(),
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
	ets:new(?ETS_ROLE_GOLDENEGG_INFO_LIST, [{keypos, 1}, set, public, named_table]),
	State = init_goldenegg_server(),
	erlang:send_after(?dump_interval,self(),dump_data),
	erlang:send_after(?dump_interval,self(), do_hibernate),
	% ?ERR("State:~w ~n",[State]),
	erlang:send_after((State#state.next_refresh_time-NowSec)*1000,self(),shop_refresh),
	case State#state.next_change_status_timestamp > NowSec of
		true->
			Ref = erlang:send_after((State#state.next_change_status_timestamp-NowSec)*1000,self(),change_status);
		false->
			?INFO("not suit nextchangestatustime exist in state:~w ~n",[State]),
			Ref = ?undefined,
			ignore
	end,
	{ok, State#state{change_status_time_ref=Ref}}.

handle_call(get_current_goodslist,_From,State)->
	Reply = {State#state.next_refresh_time,State#state.goodslist},
	{reply,Reply,State};

handle_call(get_current_status,_From,State)->
	Reply = {State#state.next_change_status_timestamp,State#state.state},
	{reply,Reply,State};

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
        
handle_info(dump_data,State)->
	erlang:send_after(?dump_interval,self(),dump_data),
	do_persist(State),
	{noreply,State};

handle_info(Info, State) ->
    case catch do_handle_info(State,Info) of
        {noreply, NewState} ->
            {noreply, NewState};
        Exeption ->
            ?ERR("Exeption:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

terminate(Reason, State) ->
	do_persist(State),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% %% ====================================================================
do_persist(State)->
	RoleEggInfoList = ets:tab2list(?ETS_ROLE_GOLDENEGG_INFO_LIST),
	% ?ERR("RoleEggInfoList:~w ~n",[RoleEggInfoList]),
	db_sql:set_etc(?DB_ETC_KEY_GOLDENEGG,{State,RoleEggInfoList}).
    
%%=======================================================================
do_handle_info(State,shop_refresh)->
	GoodsList = get_next_goodsIDList(),
	NextFreshTimeStamp = get_next_fresh_timestamp(),
	NowSec = util:now(),
	{DynamicShopID,SolidShopID} = data_goldenegg:get(data_shopID),
        SolidShopIDList = case data_shop:get(SolidShopID) of
          	?undefined->
                [];
            #data_shop{sellList=SellList}->
                SellList
        end,
        DynamicGoodsList = role_goldenegg:get_goodslist(GoodsList,DynamicShopID),
        SolidGoodsList = role_goldenegg:get_goodslist(SolidShopIDList,SolidShopID),
	Msg = #sc_goldenegg_shop{result=1,goodslist=DynamicGoodsList++SolidGoodsList,validtimestamp=NextFreshTimeStamp},
	brocast_msg_to_online_client(Msg,[]),
	erlang:send_after((NextFreshTimeStamp-NowSec)*1000,self(),shop_refresh),
	{noreply,State#state{next_refresh_time=NextFreshTimeStamp,goodslist=GoodsList}};

do_handle_info(State,change_status)->
	?ERR("change_status~n"),
	#state{begintime=BeginTimeStamp,smashendtime=SmashEndTimeStamp,exchangeendtime=ExchangeEndTimeStamp,state=Status,change_status_time_ref=OldRef} = State,
	NewState1 = case Status of
		?STATE_CLOSE-> %% 结束阶段重新读取一次配置，查看是否有新的活动配置
			init_goldenegg_server2();
		_ ->
			NewStatus = get_current_state(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
			NewNextChangeStatusTimeStamp = get_current_next_change_status_timestamp(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
			State#state{state=NewStatus,next_change_status_timestamp=NewNextChangeStatusTimeStamp}
	end,
	NowSec = util:now(),
	NewState = case NewState1#state.next_change_status_timestamp > NowSec of
		true->
			Ref = erlang:send_after((NewState1#state.next_change_status_timestamp-NowSec)*1000,self(),change_status),
			case OldRef of
				?undefined->
					ignore;
				_ ->
					erlang:cancel_timer(OldRef)
			end,
			NewState1#state{change_status_time_ref=Ref};
		false->
			?INFO("not suit nextchangestatustime exist in state:~w ~n",[State]),
			NewState1
	end,
	send_update_msg_to_online_client(NewState),
	update_role_egg_info(NewState),
	{noreply,NewState};

do_handle_info(State,reload_activity_config)->
	NowSec = util:now(),
	{BeginDateTime,SmashEndDateTime} = data_goldenegg:get(opentime),
	ExchangeInterval = data_goldenegg:get(exchange_interval),
	BeginTimeStamp = util:datetime_to_seconds(BeginDateTime),
	SmashEndTimeStamp = util:datetime_to_seconds(SmashEndDateTime),
	ExchangeEndTimeStamp = SmashEndTimeStamp + ExchangeInterval,
	Status = get_current_state(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
	NextChangeStatusTimeStamp = get_current_next_change_status_timestamp(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp),
	GoodsList = get_next_goodsIDList(),
	?ERR("NextChangeStatusTimeStamp:~w ~n",[util:seconds_to_datetime(NextChangeStatusTimeStamp)]),
	Ref = erlang:send_after((NextChangeStatusTimeStamp-NowSec)*1000,?MODULE,change_status),
	case State#state.change_status_time_ref of
		?undefined->
			ignore;	
		OldRef->
			erlang:cancel_timer(OldRef)
	end,		
	NewState = State#state{begintime=BeginTimeStamp,smashendtime=SmashEndTimeStamp,exchangeendtime=ExchangeEndTimeStamp,state=Status,next_change_status_timestamp=NextChangeStatusTimeStamp,change_status_time_ref=Ref,goodslist=GoodsList},
	send_update_msg_to_online_client(NewState),
	{noreply,NewState};


do_handle_info(State,Info)->
	?ERR("undefined Msg:~w ~n",[Info]),
	{noreply,State}.

%%========================================================================
get_status()->
	gen_server:call(?MODULE,get_current_status).

send_to_me(Msg)->
	erlang:send(goldenegg_server,Msg).

get_role_egginfo(RoleID)->
	case ets:lookup(?ETS_ROLE_GOLDENEGG_INFO_LIST,RoleID) of
		[]->
			#role_egg_info{roleID=RoleID};
		[{_Key,X}] ->
			case get_status() of
				?STATE_CLOSE->
					#role_egg_info{roleID=RoleID};
				_->
					NowSec = util:now(),
					case X#role_egg_info.validtime =< NowSec of
						true->
							X#role_egg_info{score=0,times=0};
						false->
							X
					end
			end
	end.

set_role_egginfo(EggInfo) when is_record(EggInfo,role_egg_info)->
	ets:insert(?ETS_ROLE_GOLDENEGG_INFO_LIST,{EggInfo#role_egg_info.roleID,EggInfo}).

get_current_goodslist()->
	gen_server:call(?MODULE, get_current_goodslist).

is_role_eggscore_enough(Price,RoleID)->
	case ets:lookup(?ETS_ROLE_GOLDENEGG_INFO_LIST,RoleID) of
		[]->
			false;
		[{_Key,RoleEggInfo}]->
			RoleEggInfo#role_egg_info.score >= Price
	end.

deduct_eggscore(RoleID,Value,_Type,_ArgID,_Desc)->
	case ets:lookup(?ETS_ROLE_GOLDENEGG_INFO_LIST,RoleID) of
		[]->
			?ERR("can not find role_egg_info RoleID:~w ~n",[RoleID]),
			ignore;
		[{_Key,RoleEggInfo}]->
			NowSec = util:now(),
			ScoreValidedInterval = data_goldenegg:get(score_valided_interval),
			NewRoleEggInfo = RoleEggInfo#role_egg_info{validtime=NowSec+ScoreValidedInterval,score=RoleEggInfo#role_egg_info.score-Value},
			set_role_egginfo(NewRoleEggInfo)
	end.

get_next_goodsIDList()->
	GoodsNum = data_goldenegg:get(goodsnum),
	TypeList = data_goldenegg:get(data_goods_type_list),
	Num = GoodsNum div length(TypeList),
	lists:foldl(fun(Type,Acc)->
		case data_goldenegg:get({data_goods,Type}) of
			?undefined->
				Acc;
			AllGoodsList->
				GoodsList = util:random_list(AllGoodsList,Num),
				GoodsList ++ Acc
		end
	end,[],TypeList).

get_current_state(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp)->
	case BeginTimeStamp < SmashEndTimeStamp andalso SmashEndTimeStamp < ExchangeEndTimeStamp of
		true->
			NowSec = util:now(),
			if 
				NowSec < BeginTimeStamp ->
					?STATE_CLOSE;
				NowSec >= BeginTimeStamp andalso NowSec < SmashEndTimeStamp ->
					?STATE_SMASH;
				NowSec >= SmashEndTimeStamp andalso NowSec < ExchangeEndTimeStamp->
					?STATE_EXCHANGE;
				NowSec >= ExchangeEndTimeStamp->
					?STATE_CLOSE;
				true->
					?STATE_CLOSE
			end;
		false->
			?ERR("illegal time turns B:~w SE:~w EE:~w ~n",[BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp]),
			?STATE_CLOSE
	end.

get_current_next_change_status_timestamp(BeginTimeStamp,SmashEndTimeStamp,ExchangeEndTimeStamp)->
	NowSec = util:now(),
	NextChangeStatusTimeStamp = 
	if
		BeginTimeStamp > NowSec ->
			BeginTimeStamp;
		NowSec >= BeginTimeStamp andalso NowSec < SmashEndTimeStamp ->
			SmashEndTimeStamp;
		NowSec >= SmashEndTimeStamp andalso NowSec < ExchangeEndTimeStamp->
			ExchangeEndTimeStamp;
		NowSec >= ExchangeEndTimeStamp->
			ExchangeEndTimeStamp;
		true->
			ExchangeEndTimeStamp 
	end,
	NextChangeStatusTimeStamp.

test_reload_activity_config()->
	erlang:send(?MODULE,reload_activity_config).

send_update_msg_to_online_client(State)->
	#state{state=Status,next_change_status_timestamp=NextChangeStatusTimeStamp} = State,
	Msg = #sc_goldenegg_open{status=Status,endtimestamp=NextChangeStatusTimeStamp},
	brocast_msg_to_online_client(Msg,[]).

brocast_msg_to_online_client(Msg,ExceptRoleIDList)->
	RoleIDList = [E||{E,_}<-ets:tab2list(?ETS_ROLE_ONLINE)],
    lists:foreach(fun(RoleID) ->
                          case lists:member(RoleID, ExceptRoleIDList) of
                              false ->
                              	  spawn(fun()-> catch ?unicast(RoleID,Msg) end);
                              true ->
                                  next
                          end
                  end, RoleIDList).
update_role_egg_info(State)->
	#state{state=Status} = State,
	case Status of
		?STATE_SMASH->
			ets:delete_all_objects(?ETS_ROLE_GOLDENEGG_INFO_LIST);
		?STATE_EXCHANGE->
			TabList = ets:tab2list(?ETS_ROLE_GOLDENEGG_INFO_LIST),
			lists:foreach(fun({_RoleID,RoleEggInfo})->
				set_role_egginfo(RoleEggInfo#role_egg_info{item_use_list=[]})
			end,TabList);
		?STATE_CLOSE->
			ets:delete_all_objects(?ETS_ROLE_GOLDENEGG_INFO_LIST);
		_ ->
			?ERR("undefined Status:~w ~n",[Status])	
	end.

get_next_fresh_timestamp()->
	Day = erlang:date(),
	util:datetime_to_seconds({Day,{0,0,1}})+?ONE_DAY_SECONDS.