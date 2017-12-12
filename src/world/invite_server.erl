%% @author caohongyang
%% @doc 好友进程
%% Created 2013-6-3

-module(invite_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-include("def_reward.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([i/0]).

-export([get_invite/1]).

-export([input_invite_code/2,first_pay/4]).


-define(TICK_INTERVAL, 3600). % 检查进程buff大小的时间间隔，单位：秒
%% ===================Dict Key Begin =========================
%% ===================Dict Key End   =========================

input_invite_code(RoleID, Inviter) ->
	erlang:send(?MODULE,{input_invite_code, RoleID, Inviter}).

first_pay(FirstPayRoleID, FirstPayRoleName, InviterRoleID, Gold) ->
	erlang:send(?MODULE, {first_pay, FirstPayRoleID, FirstPayRoleName, InviterRoleID, Gold}).

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
	MaxBuffSizeByM = data_setting:get(friend_process_max_buff_size),
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


%% ====================================================================
%% Internal functions
%% ====================================================================
do_handle_info({input_invite_code, RoleID, Inviter}) ->
	db_sql:add_inviteRoleID(Inviter, RoleID);
do_handle_info({first_pay, _FirstPayRoleID, FirstPayRoleName, InviterRoleID, Gold}) ->
	case check_inviter_get_firstPay(InviterRoleID) of
		true ->
			Ratio = data_invite:get(invite_first_pay_ratio),
			RewardGold = trunc(Gold*Ratio),
			Reward = [{?REWARD_GOLD,RewardGold}],
			[RatioStr]= io_lib:format("~w",[Ratio*100]),
			%% 先记录下来
			#d_invite{rewardNum=Num} = _InviterInfo = get_invite(InviterRoleID),
			Num2 = Num+1,
			db_sql:set_inviteRewardNum(InviterRoleID, Num2),
			mail_server:send_sys_mail(InviterRoleID, ?MAIL_INVITE_GUY_FIRST_PAY_REWARD, [FirstPayRoleName, RatioStr, RewardGold], "", Reward);
		GetNum ->
			?ERR("inviter ~w has got ~w first pay reward;",[InviterRoleID, GetNum])
	end;
do_handle_info(Info) ->
	throw({cannot_handle,Info}).

%% 检查玩家获得的邀请首冲奖励数量,超过配置值则返回实际获取数,否则返回true
check_inviter_get_firstPay(InviterRoleID) ->
	Times = db_sql:get_inviteRewardNum(InviterRoleID),
	case Times >= data_common:get(reward_firstPay_times) of
		true ->
			Times;
		_ ->
			true
	end.

%% 在玩家进程也调用了这个接口
get_invite(RoleID) ->
	db_sql:get_inviteInfo(RoleID).

clear_buff() ->
	lists:foreach(fun(_) ->
						  ignore				  
				  end, erlang:get()).

%% 检查本进程buff的tick
tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).
