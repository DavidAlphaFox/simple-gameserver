%% @author lixinglong
%% @doc web interface 模块

-module(web_if_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs)	->
	web_if_deps:ensure(),
	web_if_sup:start_link().

stop(_State)	->
	ok.