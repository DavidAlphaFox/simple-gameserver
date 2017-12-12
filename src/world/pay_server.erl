
%% @author caohongyang
%% @doc 充值进程
%% Created 2013-5-28

%% 已优化点：

-module(pay_server).
-behaviour(gen_server).
-compile(export_all).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-export([start_link/0,start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, do_pay_from_uc/5, do_pay_from_zz/5, do_pay_from_360/5, do_pay_from_wdj/5, do_pay_from_dk/5, do_pay_from_51cm/5,do_pay_from_mmy/5,do_pay_from_lt/5]).

-export([i/0,func/2]).


%% ===================Dict Key Begin =========================

%% ===================Dict Key End   =========================



%% @doc 获取进程状态
i() ->
	gen_server:call(?MODULE, i).

%% @doc 执行函数
func(F,Args) ->
	gen_server:call(?MODULE, {func, F,Args}).

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
	ssl:start(),
	inets:start(),
    {ok, null}.


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
handle_call({func, F, Args}, _From, State) ->
	Result = ?CATCH(apply(F,Args)),
	{reply, Result, State};
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
do_handle_info({client_msg, RoleID, #cs_role_pay_ios{receipt=Receipt,type=SrcType}=_Request}) ->
    case check_pay_ios(Receipt,RoleID,SrcType) of
        {true, _Quantity, AppItemID, Md5, Response} ->
            AppItemIDInt = binary_to_integer(AppItemID),
            Response2 = ejson:encode({Response}),
            %% ?ERR("~p~n",[Response2]),
            do_pay_ios(RoleID, AppItemIDInt, Response2, Receipt, Md5, SrcType);
        {false, Reason} ->
            ?unicast(RoleID, #sc_role_pay_ios{result=Reason,receipt=Receipt,newGoldTotalPaid=0,isGetFirstChargeReward=false})
    end;
do_handle_info({client_msg, RoleID, #cs_role_pay_91{receipt=Receipt}=_Request}) ->
	case check_pay_91(Receipt) of
		{true, _Quantity, AppItemID, Md5} ->
			AppItemIDInt = binary_to_integer(AppItemID),
			do_pay(RoleID, AppItemIDInt, Receipt, Md5, 2); 
		{false, Reason} ->
			?unicast(RoleID, #sc_role_pay_ios{result=Reason,receipt=Receipt,newGoldTotalPaid=0,isGetFirstChargeReward=false})
			end;

do_handle_info({mark_pay_info, Role0,Role2,Receipt,AppItemID,SrcType,PayGold,Ip,Mac})->
	#role{roleID=RoleID,accid=AccID,gold=Gold0,goldBonus=GoldB0,vipLevel=Vip0} = Role0,
	#role{gold=Gold2,goldBonus=GoldB2,vipLevel=Vip2,roleName=Name,deviceID=DeviceID}=Role2,
	DateTime = {Date,_} = erlang:localtime(),
	DateTime2 = db_sql:datetime(DateTime),
	Date2 = db_sql:date(Date),
	AccID2 = AccID rem ?AccidBase,
	MarkSql = io_lib:format("insert into t_pay_stastics values (null, ~w,~w,'~s','~s',~w,~w,~w,~w,~w,~w,'~s',~w,~s,'~w',~w,~w,'~s')"
						   ,[RoleID,AccID2,DateTime2,Date2,Gold0,GoldB0,Gold2,GoldB2,Vip0,Vip2,Name,SrcType,db_sql:quote(DeviceID),Ip,PayGold,AppItemID,Receipt]),
	db_sql:sql_execute_with_log(MarkSql),
	ok;

do_handle_info(Info) ->
	?ERR("handle_info function clause:request=~100p",[Info]).
    
%% do_pay_monthVIP(RoleID,AppItemIDInt,Receipt,Md5,SrcType) ->
%% 	Msg = {do_pay_monthVIP,AppItemIDInt,Receipt,Md5,SrcType},
%% 	case catch erlang:send(role_lib:regName(RoleID),Msg) of
%% 		{'EXIT',{badarg,_}} ->
%% 			do_offline_pay_monthVIP(RoleID,AppItemIDInt,Receipt,Md5,SrcType);
%% 		Msg ->
%% 			ok
%% 	end,
%% 	case data_pay:get(AppItemIDInt) of
%% 		#data_pay{payGold=PayGold} ->
%% 			%% 将这个充值收据保存，以防止重复
%% 			db_sql:add_pay_receipt(Md5,RoleID,Receipt,SrcType,PayGold);
%% 		_ ->
%% 			?ERR("充值配置错误！！！！！！@@@@@@@")
%% 	end.

%% do_pay(RoleID,AppItemIDInt,Receipt,Md5,SrcType) ->
%% 	MonthVIPPayIDList = data_monthVIP:get(monthVIPPayIDList),
%% 	case lists:member(AppItemIDInt, MonthVIPPayIDList) of
%% 		true ->
%% 			do_pay_monthVIP(RoleID,AppItemIDInt,Receipt,Md5,SrcType);
%% 		_ ->
%% 			do_pay2(RoleID, AppItemIDInt, Receipt, Md5, SrcType)
%% 	end.
do_pay(RoleID, AppItemIDInt, Receipt, Md5, SrcType)  ->
	Msg = {do_pay, AppItemIDInt, Receipt, Md5, SrcType},
	case catch erlang:send(role_lib:regName(RoleID), Msg) of
		{'EXIT',{badarg,_}} ->
			do_offline_pay(RoleID, AppItemIDInt,Receipt, Md5, SrcType);
		Msg ->
			ok
	end,
	%% 通知活动系统
	case data_pay:get(AppItemIDInt) of
		#data_pay{payGold=PayGold} ->
            %% 将这个充值收据保存，以防止重复
            db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,PayGold),
			activity_server:pay(RoleID, PayGold);
		_ ->
			?ERR("充值配置错误！！！！！！@@@@@@@")
	end.

%% do_pay_ios(RoleID, AppItemIDInt, Receipt, AppReceipt, Md5, SrcType)  ->
%% 	MonthVIPPayIDList = data_monthVIP:get(monthVIPPayIDList),
%% 	case lists:member(AppItemIDInt, MonthVIPPayIDList) of
%% 		true ->
%% 			do_pay_monthVIP(RoleID,AppItemIDInt,Receipt,Md5,SrcType);
%% 		_ ->
%% 			do_pay_ios2(RoleID, AppItemIDInt, Receipt, AppReceipt, Md5, SrcType)
%% 	end.
do_pay_ios(RoleID, AppItemIDInt, Receipt, AppReceipt, Md5, SrcType)  ->
	Msg = {do_pay, AppItemIDInt, AppReceipt, Md5, SrcType},
	case catch erlang:send(role_lib:regName(RoleID), Msg) of
		{'EXIT',{badarg,_}} ->
			do_offline_pay(RoleID, AppItemIDInt,Receipt, Md5, SrcType);
		Msg ->
			ok
	end,
	%% 通知活动系统
	case data_pay:get(AppItemIDInt) of
		#data_pay{payGold=PayGold} ->
            %% 将这个充值收据保存，以防止重复
            db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,PayGold),
			activity_server:pay(RoleID, PayGold);
		_ ->
			?ERR("充值配置错误！！！！！！@@@@@@@")
	end.

%% uc的支付接口
do_pay_from_uc(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_uc2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_uc fail Reason:~w\n",[Reason])
	end.

do_pay_from_uc2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% dl的支付接口
do_pay_from_dl(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_dl2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_uc fail Reason:~w\n",[Reason])
	end.

do_pay_from_dl2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.	

%% 猎宝的支付接口
do_pay_from_liebao(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_liebao2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{amount,Amount2}->
			do_pay_amount(RoleID,Amount2,Receipt,Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_uc fail Reason:~w\n",[Reason])
	end.

do_pay_from_liebao2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{amount,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% zz的支付接口	
do_pay_from_zz(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_zz2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_uc fail Reason:~w\n",[Reason])
	end.

do_pay_from_zz2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 360的支付接口	
do_pay_from_360(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_360_2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_360 fail Reason:~w,Receipt:~w\n",[Reason,Receipt])
	end.

do_pay_from_360_2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.


%% wdj的支付接口	
do_pay_from_wdj(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_wdj2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_wdj fail Reason:~w,Receipt:~w\n",[Reason,Receipt])
	end.

do_pay_from_wdj2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.


%% 多酷的支付接口	
do_pay_from_dk(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_dk2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_dk fail Reason:~w,Receipt:~w\n",[Reason,Receipt])
	end.

do_pay_from_dk2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 小米的支付接口	
do_pay_from_mi(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_mi2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_mi fail Reason:~w,Receipt:~w\n",[Reason,Receipt])
	end.

do_pay_from_mi2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.


%% 安智的支付接口	
do_pay_from_az(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_az2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_az fail Reason:~w,Receipt:~w\n",[Reason,Receipt])
	end.

do_pay_from_az2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% pp助手支付接口 
do_pay_from_pp(RoleID, Amount, Receipt, Sign, SrcType)->
	case do_pay_from_pp2(Amount, Sign) of
		{true , AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_pp fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_pp2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

% 快用支付接口
do_pay_from_ky(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_ky2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_ky fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_ky2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{amount,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

% 快用安卓支付接口
do_pay_from_kyand(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_kyand2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_kyand fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_kyand2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{amount,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% zr的支付接口	
do_pay_from_zr(RoleID,Amount,Receipt,Sign,SrcType) ->
	case do_pay_from_zr2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_zr fail Reason:~w\n",[Reason])
	end.

do_pay_from_zr2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 天象互动安卓支付接口  
do_pay_from_txhd_ard(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_txhd_ard2(Amount,Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_txhd_ard fail Reason:~w\n",[Reason])
    end.

do_pay_from_txhd_ard2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 天象互动安卓支付接口ext
do_pay_from_txhd_ard_ext(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_txhd_ard2_ext(Amount,Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_txhd_ard fail Reason:~w\n",[Reason])
    end.

do_pay_from_txhd_ard2_ext(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 天象互动IOS支付接口  
do_pay_from_txhd_ios(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_txhd_ios2(Amount,Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_txhd_ios fail Reason:~w\n",[Reason])
    end.

do_pay_from_txhd_ios2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% i_tools 支付接口
do_pay_from_it(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_it2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_it fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_it2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.
%% 同步推 支付接口
do_pay_from_tbt(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_tbt2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_tbt fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_tbt2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 华为支付接口
do_pay_from_hw(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_hw2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_hw fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_hw2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.


%% 91支付接口
do_pay_from_91(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_912(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_91 fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_912(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{amount,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

% sina接口
do_pay_from_sina(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_sina2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_sina fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_sina2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

% 魅族接口
do_pay_from_mz(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_mz2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_mz fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_mz2(Amount, Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false, 3};
		true ->
			List = [data_pay:get(E) || E <- data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false, Amount};
				#data_pay{payID=PayID} ->
					{true, PayID}
			end
	end.

% 37玩接口
do_pay_from_37wan(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_37wan2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_37wan fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_37wan2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end.

% 金山接口
do_pay_from_ks(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_ks2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType), 1;
        {false,Reason} ->
            ?ERR("do_pay_from_ks fail Reason:~w, Receipt:~w\n",[Reason, Receipt]), Reason
    end.

do_pay_from_ks2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,2};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,1};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 爱思接口
do_pay_from_i4(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_i42(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_i4 fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_i42(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

do_pay_from_x7(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_x72(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_x7 fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_x72(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 优酷接口
do_pay_from_yk(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_yk2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_yk fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_yk2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% PPS接口
do_pay_from_pps(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_pps2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_pps fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_pps2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 悠悠村接口
do_pay_from_uu(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_uu2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_uu fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_uu2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 顺网接口
do_pay_from_shunwang(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_shunwang2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_shunwang fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_shunwang2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 顺网IOS接口
do_pay_from_shunwang_ios(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_shunwang_ios2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_shunwang fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_shunwang_ios2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

do_pay_from_65(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_652(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_65 fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_652(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 云游飞天接口
do_pay_from_yunyou(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_yunyou2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_yunyou fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_yunyou2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 海马android接口
do_pay_from_hm_android(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_hm_android2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_hm_android fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_hm_android2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%%quick 接口
do_pay_from_quick(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_quick2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount}->
        	do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_quick fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_quick2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%%shandou 接口
do_pay_from_shandou(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_shandou2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
        	do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType)
    end.

do_pay_from_shandou2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 49游接口
do_pay_from_49you(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_49you(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_uu fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_49you(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.


% 奇天乐地ARD接口
do_pay_from_qtld_ard(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_qtld_ard2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_qtld_ard fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_qtld_ard2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.


% 奇天乐地IOS接口
do_pay_from_qtld_ios(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_qtld_ios2(Amount, Sign) of
        {true,AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_qtld_ios fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_qtld_ios2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

% 4399接口
do_pay_from_4399(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_4399_2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_4399 fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_4399_2(Amount, Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E) || E <- data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 联想接口
do_pay_from_lenovo(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_lenovo2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_sina fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_lenovo2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{amount,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% OPPO接口
do_pay_from_oppo(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_oppo2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_oppo fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_oppo2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% sogou 接口	
do_pay_from_sogou(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_sogou2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_sogo fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_sogou2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 机锋 接口
do_pay_from_jf(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_jf2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_jf fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_jf2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 应用汇接口
do_pay_from_yyh(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_yyh2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_yyh fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_yyh2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.


%% YYGame接口
do_pay_from_yyg(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_yyg2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_yyg fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_yyg2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 应用汇接口
do_pay_from_vivo(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_vivo2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_vivo fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_vivo2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.
%% 联通接口
do_pay_from_lt(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_lt2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_vivo fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_lt2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.
%% 应用宝1支付
do_pay_from_yyb1(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_yyb12(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_yyb fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_yyb12(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 应用宝支付
do_pay_from_yyb(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_yyb2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_yyb fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_yyb2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%%	偶玩儿支付
do_pay_from_ouw(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_ouw2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_ouw fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_ouw2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.
	

%% 金立

do_pay_from_jl(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_jl2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_jl fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_jl2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% xy

do_pay_from_xy(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_xy2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_xy fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_xy2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 谷果

do_pay_from_gg(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_gg2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_gg fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_gg2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 益玩

do_pay_from_cw(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_cw2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_cw fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_cw2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 乐逗
do_pay_from_ld(RoleID, Amount, Receipt, Sign, SrcType) ->
    case do_pay_from_ld2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
        {amount,Amount} ->
            do_pay_amount(RoleID,Amount,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_cw fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
    end.

do_pay_from_ld2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false,3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {amount,Amount};
                #data_pay{payID=PayID} ->
                    {true,PayID}
            end
    end.

%% 快玩儿支付
do_pay_from_kw(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_kw2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_kw fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_kw2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

do_pay_from_mzw(RoleID, Amount, Receipt, Sign, SrcType)->
	case do_pay_from_mzw2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_mzw fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_mzw2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 51畅梦支付接口
do_pay_from_51cm(RoleID, Amount, Receipt, Sign, SrcType)  ->
	case do_pay_from_51cm2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_51cm fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_51cm2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false, 3};	
		true ->
			List = [data_pay:get(X) || X <-data_pay:get_list() ],
			case lists:keyfind(Amount, #data_pay.payGold, List ) of 
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true, PayID}
			end
	end.

%%木蚂蚁支付接口
do_pay_from_mmy(RoleID, Amount, Receipt, Sign, SrcType)  ->
	case do_pay_from_mmy2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_mmy fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_mmy2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false, 3};	
		true ->
			List = [data_pay:get(X) || X <-data_pay:get_list() ],
			case lists:keyfind(Amount, #data_pay.payGold, List ) of 
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true, PayID}
			end
	end.


%% 海马接口
do_pay_from_hm(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_hm2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_hm fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_hm2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

% pptv
do_pay_from_pptv(RoleID, Amount, Receipt, Sign, SrcType) ->
	case do_pay_from_pptv2(Amount, Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_pptv fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_pptv2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 乐游天下
do_pay_from_lytx(RoleID, Amount, Receipt, Sign, SrcType)->
	case do_pay_from_lytx2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_lytx fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_lytx2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.

%% 3G门户
do_pay_from_3gmh(RoleID, Amount, Receipt, Sign, SrcType)->
	case do_pay_from_3gmh2(Amount,Sign) of
		{true, AppItemIDInt} ->
			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
		{false, Reason} ->
			?ERR("do_pay_from_3gmh fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
	end.

do_pay_from_3gmh2(Amount,Sign) ->
	case db_sql:check_pay_receipt_duplicate(Sign) of
		false ->
			{false,3};
		true ->
			List = [data_pay:get(E)||E<-data_pay:get_list()],
			case lists:keyfind(Amount, #data_pay.payGold, List) of
				false ->
					{false,Amount};
				#data_pay{payID=PayID} ->
					{true,PayID}
			end
	end.
	
%% 移动MM
do_pay_from_mm(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_mm2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_mm fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID,Reason,Receipt])
    end.

do_pay_from_mm2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount, #data_pay.payGold, List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end.

%% 电信爱游戏支付
do_pay_from_aigame(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_aigame2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false,Reason} ->
            ?ERR("do_pay_from_aigame fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_aigame2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 	

%% 支付宝快捷支付
do_pay_from_alipay(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_alipay2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_alipay fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_alipay2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 	

% i苹果
do_pay_from_iiapp(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_iiapp2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_iiapp fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_iiapp2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

% 触控
do_pay_from_ck(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_ck2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_ck fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_ck2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_xxgp(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_xxgp2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_xxgp fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_xxgp2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_cgcg(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_cgcg2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_cgcg fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_cgcg2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_kaopu(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_kaopu2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_kaopu fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_kaopu2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_kf(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_kf2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_kf fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_kf2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_pyw(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_pyw2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_pwy fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_pyw2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

%% 17173
do_pay_from_17173(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_17173_2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_17173 fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_17173_2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

%% tt
do_pay_from_tt(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_tt_2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_tt fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_tt_2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_yiyang(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_yiyang_2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_yiyang fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_yiyang_2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 

do_pay_from_xmw(RoleID,Amount,Receipt,Sign,SrcType) ->
    case do_pay_from_xmw_2(Amount, Sign) of
        {true, AppItemIDInt} ->
            do_pay(RoleID,AppItemIDInt,Receipt,Sign,SrcType);
        {false, Reason} ->
            ?ERR("do_pay_from_xmw fail, Role:~w, Reason:~p, Receipt:~p.~n", [RoleID, Reason,Receipt])
    end.

do_pay_from_xmw_2(Amount,Sign) ->
    case db_sql:check_pay_receipt_duplicate(Sign) of
        false ->
            {false, 3};
        true ->
            List = [data_pay:get(E)||E<-data_pay:get_list()],
            case lists:keyfind(Amount,#data_pay.payGold,List) of
                false ->
                    {false, Amount};
                #data_pay{payID=PayID} ->
                    {true, PayID}
            end
    end. 
%% %% 点金支付
%% do_pay_from_dj(RoleID, Amount, Receipt, Sign, SrcType) ->
%% 	case do_pay_from_dj2(Amount, Sign) of
%% 		{true, AppItemIDInt} ->
%% 			do_pay(RoleID, AppItemIDInt, Receipt, Sign, SrcType);
%% 		{false, Reason} ->
%% 			?ERR("do_pay_from_dj fail Reason:~w, Receipt:~w\n",[Reason, Receipt])
%% 	end.
%% 
%% do_pay_from_dj2(Amount,Sign) ->
%% 	case db_sql:check_pay_receipt_duplicate(Sign) of
%% 		false ->
%% 			{false,3};
%% 		true ->
%% 			List = [data_pay:get(E)||E<-data_pay:get_list()],
%% 			case lists:keyfind(Amount, #data_pay.payGold, List) of
%% 				false ->
%% 					{false,Amount};
%% 				#data_pay{payID=PayID} ->
%% 					{true,PayID}
%% 			end
%% 	end.


do_offline_pay(RoleID, AppItemIDInt, Receipt, Md5, SrcType) ->
	case db_sql:add_offlinePayLog(RoleID, AppItemIDInt, Receipt, Md5, SrcType) of
		{ok,_} ->
			ok;
		Reason ->
			?ERR("do_offline_pay aborted,RoleID=~w,reason=~1000p,appItemIDInt=~w,Receipt=~w\n",[RoleID,Reason,AppItemIDInt,Receipt])
	end.
		
%% do_offline_pay_monthVIP(RoleID,AppItemIDInt,Receipt,Md5,SrcType) ->
%% 	case db_sql:add_offlinePayMonthVIPLog(RoleID,AppItemIDInt,Receipt, Md5, SrcType) of
%% 		{ok,_} ->
%% 			ok;
%% 		Reason ->
%% 			?ERR("do_offline_pay_monthVIP aborted,RoleID=~w,reason=~1000p,appItemIDInt=~w,Receipt=~w\n",[RoleID,Reason,AppItemIDInt,Receipt])
%% 	end.

check_pay_ios(Receipt,RoleID,SrcType) ->
	%% 检查此收据是否已经处理过
	Rc = mochiweb_util:parse_qs(Receipt),
	RealReceipt = proplists:get_value("\n\t\"purchase-info\" ",Rc),
	%?ERR("realreceipt:~p~n",[RealReceipt]),
	[TmpReceipt] = string:tokens(RealReceipt,"\" "),
	%?ERR("TmpReceipt:~p~n",[TmpReceipt]),
	RealReceipt2 = base64:decode_to_string(TmpReceipt),
	%?ERR("realreceipt2:~p~n",[RealReceipt2]),
	Info = mochiweb_util:parse_qs(RealReceipt2),
	%?ERR("Info:~p~n",[Info]),
	TransID = proplists:get_value("\n\t\"transaction-id\" ",Info),
	[TransID2] = string:tokens(TransID,"\" "),
	AppItemID = proplists:get_value("\n\t\"product-id\" ",Info),
	[AppItemID2] = string:tokens(AppItemID,"\" "),
	?ERR("TransID2:~p~nAppItemID2:~p~n",[TransID2,AppItemID2]),
	Amount =
		case data_pay:get(list_to_integer(AppItemID2)) of
			#data_pay{payGold=PayGold} ->
				PayGold;
			_ ->
				0
		end,

	case db_sql:check_pay_receipt_duplicate(TransID2) of
		false ->
			{false, 3};
		true ->
			case http_check_receipt_ios(Receipt, TransID2,RoleID, SrcType) of
				{true, Data, Md5, Response} ->
					case check_pay_receipt_duplicate_inlogin(RealReceipt,TransID2,RoleID,SrcType,Amount) of
						false ->
							{false,3};
						true ->
							take_result(Data, Md5, Response)
					end;
				{false, ErrCode} ->
					{false,ErrCode}
			end
					
	end.

check_pay_91(Receipt) ->
	%% 检查此收据是否已经处理过
	ReceiptMd5 = util:md5(Receipt),
	case db_sql:check_pay_receipt_duplicate(ReceiptMd5) of
		false ->
			{false, 3};
		true ->
			case http_check_receipt_91(Receipt) of
				{true, Q, I} ->
					{true, Q, I, ReceiptMd5};
				{false, R} ->
					{false,R}
			end
	end.


to_binary(Bin) when is_binary(Bin) ->
	Bin;
to_binary(IoList) when is_list(IoList) ->
	list_to_binary(IoList).

http_check_receipt_ios(Receipt, Md5,RoleID, SrcType) ->
	Response = http_request_ios(Receipt,RoleID,3),
	%% ?ERR("IOS pay Resopnse:~w",[Response]),
	Status = get_value(Response, <<"status">>),
	%% ?ERR("status:~p~n",[Status]),
	if Status == 0 ->
		   {Data} = get_value(Response, <<"receipt">>),
		   ResAppBid = get_value(Data, <<"bid">>),
		   AppBid =
               case SrcType of
                   0 ->
                       "net.crimoon.pm.pikaqiu";
                   1 ->
                       "com.skymoons.qbpikaqiu"
               end,
		   case (to_binary(ResAppBid) =:= to_binary(AppBid)) of 
			true -> 
				{true, Data, Md5, Response};
				%% take_result(Data, Md5, Response);
			false ->
				{false, 4}
		   end;
	   Status =:= false ->
		   {false, 2};
	   true ->
		   {false,Status}
	end.

http_check_receipt_91(Receipt) ->
	AppID = integer_to_list(data_setting:get(app_id_91)),
	AppKey = data_setting:get(app_key_91),
	sdk91:check_order_serial(AppID, AppKey, Receipt).

http_request_ios(Receipt,RoleID, Count) ->
	if Count =:= 0 ->
			?ERR("http_request_ios timeout:~p~n~p~n",[RoleID,Receipt]),
			db_sql:set_failedPayLog(RoleID,Receipt),
			{error,time_is_empty};
		true ->
			ReceiptBin = base64:encode(Receipt),
			RequestData = ejson:encode({[{<<"receipt-data">>,ReceiptBin}]}),
			%% 正式地址
			%% Url = "https://buy.itunes.apple.com/verifyReceipt",
			%测试地址
			 Url = "https://sandbox.itunes.apple.com/verifyReceipt",
			case httpc:request(post, {Url,[], "",RequestData}, [{timeout, 3000}], []) of
				{ok, {_,_, ResponseData}} ->
					{Response} = ejson:decode(ResponseData),
					Response;
				{error,_} ->
					http_request_ios(Receipt,RoleID,Count-1);
				Err ->
					?ERR("http_request_ios unkown_response:~p~n~p~n~p~n",[Err,RoleID,Receipt]),
					db_sql:set_failedPayLog(RoleID,Receipt),
					{error,Err}
			end
	end.

%% check_pay_receipt_duplicate_inlogin(Receipt,ReceiptMD5,RoleID,SrcType,Amount) ->
%% 	ReceiptBase64 = erlang:binary_to_list(base64:encode(Receipt)),
%%     AccID = db_sql:get_role_accid(RoleID),
%% 	case catch gen_server:call({global, util:get_pay_server()}, {check_receipt, ReceiptBase64,ReceiptMD5,RoleID,SrcType,AccID,Amount}) of
%% 		true ->
%%             true;
%%         false ->
%%             false;
%% 		Error ->
%%             ?ERR("Error:~w", [Error]),
%% 			false
%% 	end.

check_pay_receipt_duplicate_inlogin(Receipt,ReceiptMD5,RoleID,SrcType,Amount) ->
	Url0 = data_setting:get(account_check_url),
	Url = Url0++"/checkiosreicept",
	RoleID2 = integer_to_list(RoleID),
	ReceiptBin = base64:encode(Receipt),
	RoleAccID = db_sql:get_role_accid(RoleID),
	RequestData = ejson:encode({[{<<"receipt">>,ReceiptBin},{<<"md5">>,list_to_binary(ReceiptMD5)}
								,{<<"roleid">>,list_to_binary(RoleID2)},{<<"accid">>,RoleAccID}
								,{<<"srcType">>,SrcType},{<<"amount">>,Amount}]}),
	case httpc:request(post, {Url,[], "application/x-www-form-urlencoded",RequestData}, [{timeout, 3000}], []) of
		{ok, {_,_,Content}} ->
			{Content2} = ejson:decode(Content),
			Result = get_value(Content2, <<"result">>),
			if Result == 1 ->
					true;
				true ->
					false
			end;
		_ ->
			false
	end.
		
do_pay_amount(RoleID, Amount, Receipt, Md5, SrcType)  ->
    Msg = {do_pay_amount, Amount, Receipt, Md5, SrcType},
    case catch erlang:send(role_lib:regName(RoleID), Msg) of
        {'EXIT',{badarg,_}} ->
            do_offline_pay_amount(RoleID, Amount,Receipt, Md5, SrcType);
        Msg ->
            ok
    end,
    %% 將這個儲值收據保存，以防止重複
    db_sql:add_pay_receipt(Md5, RoleID, Receipt,SrcType,Amount),
    %% 通知活動系統
    activity_server:pay(RoleID, Amount).

do_offline_pay_amount(RoleID, Amount, Receipt, Md5, SrcType) ->
    case db_sql:add_offlinePayAmountLog(RoleID, Amount, Receipt, Md5, SrcType) of
        {ok,_} ->
            ok;
        Reason ->
            ?ERR("do_offline_pay_amount aborted,RoleID=~w,reason=~1000p,amount=~w,Receipt=~w\n",[RoleID,Reason,Amount,Receipt])
    end.
		   
take_result(Data, Md5, Response) ->
	{true, get_value(Data, <<"quantity">>), get_value(Data, <<"product_id">>), Md5, Response}.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.
