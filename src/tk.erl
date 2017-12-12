%% @author caohongyang
%% @doc 服务器总控制接口
%% Created 2013-2-19


-module(tk).
-include("common.hrl").

-export([start/0,stop/0]).

-define(APP, [sasl, crypto, inets, logger, tool, msg, emysql, db, behavior, worker,world, carlos, doublematch, family_world, role, gateway, web_if]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->	
    App =  
        case data_setting:get(server_type) of
            normal ->       ?APP;
			family_cross -> ?APP;
			family_cross_master -> ?APP;
            _ -> lists:delete(family_world, ?APP)
        end,
	tk_misc:start_applications(App),
	%% 启动后对虚拟机整体垃圾回收一次
	lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, erlang:processes()).
	
stop() ->
    ets:insert(?ETS_STOP_FLAG, {flag, true}),
	%% 先t所有玩家,并关闭游戏入口。
	application:stop(gateway),
	%% 等待10秒，等玩家进程保存数据
	timer:sleep(10*1000),
	%% 关闭mochiweb接口
	application:stop(web_if),
	%% 关闭玩家application
	application:stop(role),
	%% 关闭世界application，并等待10秒写完数据
	timer:sleep(1000),
	application:stop(family_world),
	application:stop(carlos),
    application:stop(doublematch),
	application:stop(world),
	application:stop(worker),
	timer:sleep(10*1000),
	%% 
	tk_misc:stop_applications( [sasl, crypto, inets, logger, tool, emysql, db, behavior,msg]),
	timer:sleep(3*1000).
                                                                        
	




