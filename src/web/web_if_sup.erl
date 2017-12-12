%% @author lixinglong
%% @doc web interface supervisor
%% @notice mochiweb使用的端口信息在setting.config中配置

-module(web_if_sup).
-behavour(supervisor).

-export([init/1]).

%%API functions
-export([]).
-export([start_link/0]).

start_link()	->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])	->
    Web = web_specs(web_web),
	Processes = [Web],
	Strategy = {one_for_one, 10, 10},
	{ok,{Strategy, lists:flatten(Processes)}}.

web_specs(Mod) ->
	{_, Port1 ,IP1} = data_setting:get(passinfo),
	{Port2,IP2} = data_setting:get(pay_port),
	WebConfig = [{port1,[{ip, IP1},{port, Port1}]},
				 {port2,[{ip, IP2},{port, Port2}]},
				 {docroot, web_if_deps:local_path(["deps", "mochiweb"])}],
	{Mod,
	 {Mod, start, [WebConfig]},
	 permanent, 5000, worker, dynamic}.