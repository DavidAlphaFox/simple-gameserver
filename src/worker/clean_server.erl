%% @author lixinglong
%% @doc @todo Add description to clean_server.


-module(clean_server).
-behaviour(gen_server).
-include("common.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).


do_clean()->
	erlang:send(?MODULE,do_clean).

start() ->
	{ok,_}=
    supervisor:start_child(worker_sup, 
                           {?MODULE,
                            {?MODULE, start_link, []},
                            permanent, 600000, worker, [?MODULE]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	case data_setting:get(server_type) of
		familyfight ->
			tick();
		_ ->
			ignore
	end,
	{ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
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
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
	do_handle_info(Info),
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_handle_info(do_clean)->
	Pids = get_waiting_pid_list(),
	PidsInfo = get_waiting_pid_info(Pids),
	case PidsInfo of
		[] ->
			ignore;
		_ ->
			send_msg:direct_by_name(clean_master,clean_server,{waiting_pids,data_setting:get(server_id),PidsInfo}),
			Roles = get_unable_login_roles(Pids),
			record_role_info(PidsInfo,Roles),
			[do_offline(RoleID)||RoleID<-Roles]
	end;

do_handle_info({waiting_pids,ServerID,PidsInfo}) ->
	D1 = lists:flatten(io_lib:format("~p",[PidsInfo])),
	Data = io_lib:format("insert into log_waiting_pids values (null, ~s,'~p',null);",[db_sql:quote(D1),ServerID]),
	db_sql:sql_execute_with_log(Data);

do_handle_info(tick) ->
	tick(),
	send_msg:broadcast(clean_server,do_clean),
	ok;
	
do_handle_info(_) ->
	ignore.

tick()->
	erlang:send_after(3600000, self(), tick).

record_role_info(Info,Roles)->
	D1 = lists:flatten(io_lib:format("~p",[Info])),
	Data = io_lib:format("insert into log_waiting_pids values (null,~s,'~p',null);", [db_sql:quote(D1),Roles]),
	db_sql:sql_execute_with_log(Data).

do_offline(RoleID) ->
	case role_lib:is_online(RoleID) of
		true ->
			exit(erlang:whereis(erlang:list_to_atom("role"++integer_to_list(RoleID))),kill),
			ets:delete(role_state,RoleID),
			ets:delete(ets_role_online,RoleID);
		_ ->
			ignore
	end.

get_waiting_pid_info(Pids) ->
	[process_info(Pid,[registered_name,message_queue_len,current_stacktrace])||Pid<-Pids].

get_waiting_pid_list()->
	lists:foldl(fun(P,Acc)->
						case process_info(P,message_queue_len) of
							{_,X} when X > 300 ->
								[P|Acc];
							_ ->
								Acc
						end
				end, [], erlang:processes()).

get_unable_login_roles(Pids) ->
	lists:foldl(fun(P,Acc)->
						Socket_send = socket_send_function(),
						case process_info(P,current_stacktrace) of
							{current_stacktrace,List} ->
								case hd(List) of
									Socket_send ->
										RoleID = 
											case catch get_roleID(P) of
												X when is_integer(X) ->
													X;
												_ ->
													0
											end,
										[RoleID|Acc];
									_ ->
										Acc
								end;
							_ ->
								Acc
						end
				end, [], Pids).

socket_send_function()->
	case data_setting:get(is_release) of
		true ->
			{prim_inet,send,3,[]};
		_->
			{clean_server,send,3,[]}
		  end.

get_roleID(P) ->
	{_,RName} = process_info(P,registered_name),
	list_to_integer(atom_to_list(RName) -- "role").


stacktrace()->
	{current_stacktrace,[{prim_inet,send,3,[]},
						 {role_ger,'-cs_ger_info/1-fun-0-',4,
						  [{file,"src/role/ger/role_ger.erl"},{line,61}]},
						 {lists,foldl,3,[{file,"lists.erl"},{line,1248}]},
						 {role_server,handle_info,2,
						  [{file,"src/role/role_server.erl"},{line,302}]},
						 {gen_server,handle_msg,5,
						  [{file,"gen_server.erl"},{line,604}]},
						 {proc_lib,init_p_do_apply,3,
						  [{file,"proc_lib.erl"},{line,239}]}]}.

send(_)->
	Info = erlang:iolist_to_binary(proto:encode({sc_account_heart,1449713910})),
	case catch process_info(role_lib:gw(role_data:get_roleID()),dictionary) of
	{dictionary, [{socket,Socket}]} ->
		catch send(Socket,Info,[]);
	_ ->
	ignore
end.

send(S, Data, OptList) when is_port(S), is_list(OptList) ->
    try my_command(S,Data,OptList) of
	false -> % Port busy and nosuspend option passed
	    {error,busy};
	true ->
		receive 
			{inet_reply,_,_}=X ->
				?ERR("receive msg2:~w",[X])
		end,
		receive
			{inet_reply,S,Status} ->
				?ERR("receive msg3:~w",[Status]),
				Status
		end
	catch
		error:_Error ->
			{error,einval}
	end.

send(S, Data) ->
    send(S, Data, []).

my_command(Port, Data, Flags) ->
    case case erts_internal:port_command(Port, Data, Flags) of
	     Ref when erlang:is_reference(Ref) -> 
			 receive {Ref, Res} -> Res 
			 end;
	     Res -> Res
	 end of
	Bool when Bool == true; Bool == false -> Bool;
	Error -> erlang:error(Error, [Port, Data, Flags])
    end.