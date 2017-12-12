%% @author zcl
%% @doc 签到
%% Created 2015-11-30
-module(sign_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-include("def_carlos.hrl").

-export([start_link/0,start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ===================Dict Key Begin =========================
-define(dump_interval,300*1000).
-define(MEGACLIP_REWARD_TYPE,10).
-define(KEYCLIP_REWARD_TYPE,11).
-define(DAILY_SIGN_REWARD_TYPE,1).
-define(ACC_SIGN_REWARD_TYPE,2).
-define(RESET_DAY,1).
-define(RESET_MONTH,2).
-define(RESET_YEAR,3).
%% ===================Dict Key End   =========================

%% ===================record define begin=====================
-record(state,{current_clip_gertypeID=0,already_generate_clip_gertypeIDlist=[]}).
%% ===================record define end=======================



%% =========================================================================================

%%初始化签到服务进程
init_sign_server()->
	case db_sql:get_etc(?DB_ETC_KEY_SIGN_INFO) of
		SignInfo when is_record(SignInfo,state) ->
			init_reset_info(),
			SignInfo;
		_ ->
			init_sign_server2()
	end.

init_sign_server2()->
	CurrentGerTypeID = case data_sign:get(data_mega_list) of
		List when is_list(List)->
			lists:nth(random:uniform(length(List)),List);
		Other->
			?ERR("data_sign.config文件中中data_mega_list配置:~w 错误~n",[Other]),
			0
	end,
	init_reset_info(),
	#state{current_clip_gertypeID=CurrentGerTypeID}.

init_reset_info()->
	NowSec = util:now(),
	DayResetTimeStamp = get_next_day_reset_timestamp(),
	erlang:send_after((DayResetTimeStamp-NowSec)*1000,?MODULE,reset_day).
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
	State = init_sign_server(),
	erlang:send_after(?dump_interval,self(),dump_data),
	erlang:send_after(?dump_interval, self(), do_hibernate),
	{ok, State}.

handle_call(get_current_gerType,_From,State)->
    {reply,State#state.current_clip_gertypeID,State};

handle_call({get_sign_reward,SignInfo,Type},_From,State)->
	R = get_reward(State#state.current_clip_gertypeID,SignInfo,0,Type),
	{reply,R,State};

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
	db_sql:set_etc(?DB_ETC_KEY_SIGN_INFO,State).


%%=======================================================================
do_handle_info(State,reset_day)->
	NewState = hook_zero_clock(State),
	{noreply,NewState};

do_handle_info(State,{reset,Type})->
	NewState = do_reset(Type,State),
	{noreply,NewState};

do_handle_info(State,Info)->
	?ERR("undefined Msg:~w ~n",[Info]),
	{noreply,State}.

%%返回下次每日重置时间戳
get_next_day_reset_timestamp()->
	case data_sign:get(data_daily_reset_test) of
		true->
			util:now()+data_sign:get(data_daily_reset_interval);
		false->
			Today = erlang:date(),
			util:datetime_to_seconds({Today,{0,0,0}})+?ONE_DAY_SECONDS
	end.

%%返回下次每月重置时间戳
get_next_month_reset_timestamp()->
	case data_sign:get(data_month_reset_test) of
		true->
			util:now()+data_sign:get(data_month_reset_interval);
		false->
			Today = erlang:date(),
			NextMonth = get_next_month(Today),
			util:datetime_to_seconds({NextMonth,{0,0,1}})
	end.

%% 返回下次年重置时间戳
get_next_year_reset_timestamp()->
	case data_sign:get(data_year_reset_test) of
		true->
			util:now()+data_sign:get(data_year_reset_interval);
		false->
			{Year,_Month,_Day} = erlang:date(),
			util:datetime_to_seconds({{Year+1,1,1},{0,0,1}})
	end.


get_next_month({Year,Mon,_Day})->
    {NewYear,NewMon} = case Mon +1 > 12 of
        true->
            {Year+1,1};
        false->
            {Year,Mon+1}
    end,
    {NewYear,NewMon,1}.

get_next_gerTypeID(AlreadyGenerateList,AllList)->
	RemainList = AllList -- AlreadyGenerateList,
	case RemainList of
		[]->
			?ERR("already_generate_list:~w alllist:~w ~n",[AlreadyGenerateList,AllList]),
			0;
		_ ->
			lists:nth(random:uniform(length(RemainList)),RemainList)
	end.


%%根据当前的mega精灵ID生成本月的奖励列表
generate_sign_rewardlist(CurrentGerTypeID,Type,SignInfo)->
	case get_reward_list(CurrentGerTypeID,Type,SignInfo) of
		[]->
			[];
		List->
			[#p_sign_reward{sign_count=Day,sign_reward=role_reward:transform2p_reward_view(R,[])}||{Day,R}<-List]
	end.

get_reward_list(CurrentGerTypeID,Type,SignInfo)->
	List = case Type of
		?DAILY_SIGN_REWARD_TYPE->
			data_sign:get(data_sign_reward_list);
		?ACC_SIGN_REWARD_TYPE->
			List1 = data_sign:get(data_sign_acc_reward_list),
			choose_acc_reward(SignInfo,List1);
		_ ->
			?ERR("undefined tyep:~w ~n",[Type]),
			[]
	end,
	case List of
		?undefined->
			[];
		List->
			[{Day,[fix_clip_reward(E,CurrentGerTypeID)||E<-RewardList]}||{Day,RewardList}<-List]
	end.

fix_clip_reward({?MEGACLIP_REWARD_TYPE,Num},CurrentGerTypeID)->
	{6,role_awake:get_mega_clip_id(CurrentGerTypeID),Num};
fix_clip_reward({?KEYCLIP_REWARD_TYPE,Num},CurrentGerTypeID)->
	{6,role_awake:get_key_clip_id(CurrentGerTypeID),Num};
fix_clip_reward(Other,_CurrentGerTypeID)->
	Other.


get_reward(CurrentGerTypeID,SignInfo,AccRewardCount,Type)->
	case get_reward_list(CurrentGerTypeID,Type,SignInfo) of
		[]->
			?ERR("sign_reward_list is null~n"),
			[];
		RewardList->
			Count = case Type of
				?ACC_SIGN_REWARD_TYPE->
					AccRewardCount;
				?DAILY_SIGN_REWARD_TYPE->
					SignInfo#sign_info.sign_time_count;
				_ ->
					0
			end,
			case lists:keyfind(Count,1,RewardList) of
				false->
					?ERR("sign_time_count is illegal： SignInfo:~w RewardList:~w ~n",[SignInfo,RewardList]),
					[];
				FindOne->
					FindOne
			end
	end.

%%新的一天来到
hook_zero_clock(State)->
	?INFO("hook_zero_clock:State:~w ~n",[State]),
	NowSec = util:now(),
	{_Year,Month,Day} = erlang:date(),
	DayResetTimeStamp = get_next_day_reset_timestamp(),
	erlang:send_after((DayResetTimeStamp-NowSec)*1000,?MODULE,reset_day),
	State1 = case Day =:= 1 of
		true->
			#state{current_clip_gertypeID=CurrentGerTypeID,already_generate_clip_gertypeIDlist=AlreadyGenerateList}=State,
			NextGerTypeID = get_next_gerTypeID([CurrentGerTypeID|AlreadyGenerateList],data_sign:get(data_mega_list)),
			State#state{current_clip_gertypeID=NextGerTypeID,already_generate_clip_gertypeIDlist=[CurrentGerTypeID|AlreadyGenerateList]};
		false->
			State
	end,
	case Month=:=1 andalso Day =:= 1 of
		true->
			NextGerTypeID1 = get_next_gerTypeID([],data_sign:get(data_mega_list)),
			State1#state{current_clip_gertypeID=NextGerTypeID1,already_generate_clip_gertypeIDlist=[]};
		false->
			State1
	end.

do_reset(Type,State)->
	case Type of
		?RESET_DAY->
			State;
		?RESET_MONTH->
			#state{current_clip_gertypeID=CurrentGerTypeID,already_generate_clip_gertypeIDlist=AlreadyGenerateList}=State,
			NextGerTypeID = get_next_gerTypeID([CurrentGerTypeID|AlreadyGenerateList],data_sign:get(data_mega_list)),
			State#state{current_clip_gertypeID=NextGerTypeID,already_generate_clip_gertypeIDlist=[CurrentGerTypeID|AlreadyGenerateList]};
		?RESET_YEAR->
			NextGerTypeID1 = get_next_gerTypeID([],data_sign:get(data_mega_list)),
			State#state{current_clip_gertypeID=NextGerTypeID1,already_generate_clip_gertypeIDlist=[]};
		_ ->
			?ERR("undefined Type:~w ~n",[Type]),
			State
	end.

choose_acc_reward(_SignInfo,AllRewardList)->
    {_Year,Month,_Day} = erlang:date(),
	MonthNum = role_sign:get_month_day_num(Month),
	[E||{Day,_P}=E<-AllRewardList,Day=<MonthNum].
	% List2 = lists:foldl(fun({Day,_P}=E,Acc)->
	% 	case Day >= SignInfo#sign_info.is_get_acc_sign_reward andalso Day =< SignInfo#sign_info.sign_time_count of
	% 		true->
	% 			[E|Acc];
	% 		false->
	% 			Acc
	% 	end
	% end,[],AllRewardList),
	% List2.
	% AllRewardList.

send_to_me(Msg)->
	erlang:send(?MODULE,Msg).

%%=============================================================================================================================%%
test_reset_day()->
	Msg = {reset,?RESET_DAY},
	send_to_me(Msg).

test_reset_month()->
	Msg = {reset,?RESET_MONTH},
	send_to_me(Msg).

test_reset_year()->
	Msg = {reset,?RESET_YEAR},
	send_to_me(Msg).