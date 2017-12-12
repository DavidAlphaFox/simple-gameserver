%% application: logger
-module(logger_app).

-behaviour(application).

-export([
	 start/0,
	 start/2,
	 stop/1
        ]).

-export([]).

start() ->
	application:start(server_logger).

start(_, _) ->
	application:start(sasl),
	ErrorLogHandlers = gen_event:which_handlers(error_logger),
	lists:foreach(fun(H) ->gen_event:delete_handler(error_logger, H, []) end, ErrorLogHandlers),
	gen_event:add_handler(error_logger,logger_handler, []),
	LogLevel = get_log_level(),
	logger_gen:set(LogLevel),
	{ok, SupPid} = logger_sup:start_link(),
	{ok, SupPid}.
	
get_log_level() ->
	case file:consult(filename:join([tk_config:root_dir(),"setting","setting.config"])) of
		{ok, KVList} ->
			case lists:keyfind(log_level, 1, KVList) of
				false ->
					3;
				{_,LogLevel} ->
					LogLevel
			end;
		_ ->
			3
	end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

