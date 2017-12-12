%% @author caohongyang
%% @doc Supervisor of tcp listener
%% Created 2013-2-20


-module(gateway_tcp_listener_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-export([start_link/1]).

start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([Port, AcceptorNum]) ->
	
    {ok,{{one_for_all,10,10}, [            
                {
                    gateway_tcp_acceptor_sup,
                    {gateway_tcp_acceptor_sup, start_link, []},
                    permanent,
                    100,
                    supervisor,
                    [gateway_tcp_acceptor_sup]
                },       
                {
                    gateway_tcp_client_sup,
                    {gateway_tcp_client_sup, start_link, []},
                    permanent,
                    100,
                    supervisor,
                    [gateway_tcp_client_sup]
                },
                {
                    gateway_tcp_listener,
                    {gateway_tcp_listener, start_link, [Port, AcceptorNum]},
                    transient,
                    100,
                    worker,
                    [gateway_tcp_listener]
                }
            ]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


