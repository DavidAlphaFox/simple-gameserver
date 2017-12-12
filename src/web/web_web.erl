%% @author lixinglong
%% @doc 启动mochiweb, 调用handle模块处理http请求,start中将不同的接口指派给不同的socket_server
%% @notice mochiweb的接收池设置：mochiweb_socket_server模块中，#mochiweb_socket_server{acceptor_pool_size}

-module(web_web).

-export([start/1, start/2, stop/0, port1_loop/2,port2_loop/2]).

%% External API
start(Options,_)  ->
  start(Options).

start(Options) ->
    {DocRoot, OptionsT} = get_option(docroot, Options),
	[Options1,Options2] = depart_options(OptionsT),
	Port1_Loop = fun (Req) ->
						  ?MODULE:port1_loop(Req, DocRoot)
				 end,
	Port2_Loop = fun (Req) ->
						  ?MODULE:port2_loop(Req, DocRoot)
				 end,
	mochiweb_http:start([{name, port1}, {loop, Port1_Loop} | Options1]),
    mochiweb_http:start([{name, port2}, {loop, Port2_Loop} | Options2]).


stop() ->
    mochiweb_http:stop(port1),
	mochiweb_http:stop(port2).

%% 礼包功能web接口处理，ip，port 配置在setting.config中
port1_loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    try
        web_if_handle:handle(Method, Path, Req, DocRoot)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% 支付功能web接口处理， ip，port 配置在setting.config中
port2_loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    try
        web_if_handle:handle(Method, Path, Req, DocRoot)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

depart_options(Options)->	
	Options1 = proplists:get_value(port1, Options),
	Options2 = proplists:get_value(port2, Options),
	[Options1,Options2].
