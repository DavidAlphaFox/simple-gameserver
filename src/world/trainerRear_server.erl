%% @author zcl
%% @doc 签到
%% Created 2017-9-4
-module(trainerRear_server).
-behaviour(gen_server).
-compile(export_all).
-include("def_role.hrl").


-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================
-define(dump_interval,300*1000).
-define(SPECIAL_BOX_OPENED,1). %%特殊宝箱可以开
-define(SPECIAL_BOX_CLOSED,0). %%特殊宝箱已经开过
%% ===================Dict Key End   =========================

%% ===================record define begin=====================
-record(state,{}).
-record(box_unit,{boxID=0,boxState=?SPECIAL_BOX_OPENED,boxFreshTime=0,freshRef=?undefined}).
%% ===================record define end=======================



%% =========================================================================================

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
	init_trainerRear_server(),
	erlang:send_after(?dump_interval,self(),dump_data),
	erlang:send_after(?dump_interval, self(), do_hibernate),
	{ok, #state{}}.

handle_call({update_box_info,BoxID,BoxState,IsFreshTime,OperateType},_From,State)->
	% ?ERR("Call Msg:~w ~n",[Msg]),
	Reply = do_update_box_info(BoxID,BoxState,IsFreshTime,OperateType),
	{reply,Reply,State};

handle_call(Request, _From, State) ->
	?ERR("Request:~w~n",[Request]),
	Reply=ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
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
            ?ERR("Exception:~w~n Info:~w~n State:~w~n", [Exeption, Info, State]),
            {noreply, State}
    end.

terminate(Reason, State) ->
	do_persist(State),
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% %% ====================================================================
do_persist(_State)->
	List = ets:tab2list(?ETS_TRAINERREAR),
	db_sql:set_etc(?DB_ETC_KEY_TREAREAR,List).

do_handle_info(State,{fresh_box,BoxID,_BoxState})->
	case get_box_info(BoxID) of
		?undefined->
			ignore;
		#box_unit{boxFreshTime=FreshTime}->
			Now = util:now(),
			case FreshTime > Now of 
				true->
					ignore;
				_->
					SpecialBoxList = data_trainerRear:get(special_box_list),
					case lists:keyfind(BoxID,1,SpecialBoxList) of
						false->
							%%重新查看配置表，不存在的boxID配置，直接取消对应的刷新
							ets:delete(?ETS_TRAINERREAR,BoxID),
							ignore;
						{BoxID,Interval}->
							add_box(BoxID,?SPECIAL_BOX_OPENED,fresh_time_transfor(Interval))
					end
			end
	end,
	{noreply,State};
do_handle_info(State,Info)->
	?ERR("undefined Info:~w~n",[Info]),
	{noreply,State}.							

%%=======================================================================
init_trainerRear_server()->
	case db_sql:get_etc(?DB_ETC_KEY_TREAREAR) of
		[]->
			SpecialBoxList = data_trainerRear:get(special_box_list),
			[add_box(BoxID,?SPECIAL_BOX_OPENED,fresh_time_transfor(Interval))||{BoxID,Interval}<-SpecialBoxList];
		BoxList ->
			SpecialBoxList = data_trainerRear:get(special_box_list),
			ExistBoxIDList = [E||#box_unit{boxID=E}<-BoxList],
			NotExistBoxIDList = [E||{BoxID,_Interval}=E<-SpecialBoxList, not lists:member(BoxID,ExistBoxIDList)],
			[add_box(BoxID,?SPECIAL_BOX_OPENED,fresh_time_transfor(Interval))||{BoxID,Interval}<-NotExistBoxIDList],
			init_trainerRear_server2(BoxList)
	end.
init_trainerRear_server2([])->
	ignore;
init_trainerRear_server2([#box_unit{freshRef=Ref,boxFreshTime=FreshTime,boxID=BoxID,boxState=State}|T])->
	case Ref of
		?undefined->
			ignore;
		_->
			?ERR("Ref:~w ~n",[Ref]),
			erlang:cancel_timer(Ref)
	end,
	Now = util:now(),
	case Now > FreshTime of
		true->
			SpecialBoxList = data_trainerRear:get(special_box_list),
			case lists:keyfind(BoxID,1,SpecialBoxList) of
				false->
					ignore;
				{BoxID,Interval}->
					%%直接重新设置一个
					add_box(BoxID,?SPECIAL_BOX_OPENED,fresh_time_transfor(Interval))
			end;
		false->
			%%依然使用上一次设定的刷新时间
			add_box(BoxID,State,FreshTime-Now)
	end,
	init_trainerRear_server2(T).

add_box(BoxID,State,Interval)->
	Ref = erlang:send_after(Interval*1000,?MODULE,{fresh_box,BoxID,State}),
	Now = util:now(),
	BoxInfo = #box_unit{boxID=BoxID,boxState=State,boxFreshTime=Now+Interval,freshRef=Ref},
	ets:insert(?ETS_TRAINERREAR,BoxInfo).

get_box_info(BoxID)->
	case ets:lookup(?ETS_TRAINERREAR,BoxID) of
		[]->
			?undefined;
		[X]->
			X
	end.

fresh_time_transfor({Day,Hour,Min})->
	Day*?ONE_DAY_SECONDS + Hour*?ONE_HOUR_SECONDS + Min*60.

get_box_state(BoxID)->
	case get_box_info(BoxID) of
		?undefined->	
			false;
		#box_unit{boxState=?SPECIAL_BOX_CLOSED}->
			false;
		#box_unit{boxState=?SPECIAL_BOX_OPENED}->
			true;
		_->
			false
	end.

do_update_box_info(BoxID,State,IsFreshTime,test)->
	case get_box_info(BoxID) of
		?undefined->
			true;
		#box_unit{freshRef=Ref}=Box ->
			case IsFreshTime of
				false->
					ets:insert(?ETS_TRAINERREAR,Box#box_unit{boxState=State});
				true->
					SpecialBoxList = data_trainerRear:get(special_box_list),
					case lists:keyfind(BoxID,1,SpecialBoxList) of
						false->
							ets:delete(?ETS_TRAINERREAR,BoxID),
							false;
						{BoxID,Interval}->
							case Ref of
								?undefined->
									ignore;
								_->
									erlang:cancel_timer(Ref)
							end,
							add_box(BoxID,State,fresh_time_transfor(Interval))
					end
			end
	end;

%%玩家状态不能更改刷新
do_update_box_info(BoxID,_State,_IsFreshTime,role)->
	case get_box_info(BoxID) of
		?undefined->
			true;
		#box_unit{boxState=?SPECIAL_BOX_OPENED}=Box->
			ets:insert(?ETS_TRAINERREAR,Box#box_unit{boxState=?SPECIAL_BOX_CLOSED}),
			true;
		_->
			false
	end.

%%该函数是玩家进程使用的，如果状态已经是SPECIAL_BOX_CLOSED状态，将会返回false
update_box_info(BoxID)->
	gen_server:call(?MODULE,{update_box_info,BoxID,?SPECIAL_BOX_CLOSED,false,role}).

%%=============================================================================================================================%%

%%该函数用于测试使用,能够强行更改状态
test_update_box_info(BoxID,State,IsFreshTime)->
	case lists:member(State,[?SPECIAL_BOX_CLOSED,?SPECIAL_BOX_OPENED]) of
		true->
			gen_server:call(?MODULE,{update_box_info,BoxID,State,IsFreshTime,test});
		false->
			?ERR("iilegal State:~w ~n",[State]),
			false
	end.

gain_unique_box(BoxID)->
	case get_box_state(BoxID) of
		true->
			update_box_info(BoxID);
		false->
			false
	end.
