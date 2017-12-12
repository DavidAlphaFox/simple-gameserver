%% @author lixinglong
%% @doc 处理玩家登录后注册与注销进程流程

-module(role_state).
-export([init/0, login/2, logoff_without_flush/1, logoff_with_flush/2]).
-compile(export_all).
-define(REG_JUDG_TABLE, ?MODULE).
-define(DUMP_NUM, 50).
-define(DUMP_WAIT_TIME_OUT, 300).
-include("common.hrl").
-compile(export_all).

init()	->
	ets:new(?REG_JUDG_TABLE, [public, named_table]),
	ok.

login(RoleID, Pid)	->
	login(RoleID, Pid, 0).

login(RoleID, Pid, N)	->
	case ets:lookup(?REG_JUDG_TABLE, RoleID)	of
		[]	-> 
			ets:insert(?REG_JUDG_TABLE, {RoleID, 0}),
			case catch erlang:register(role_lib:regName(RoleID), Pid) of
                true ->
                    true;
                Reason ->
                    ?ERR("Reason:~w,RoleID:~w,Pid:~w", [Reason,RoleID,Pid]),
                    ets:delete(?REG_JUDG_TABLE,RoleID),
                    false
            end;
		_	->
			if 
				N > 20 ->
					false;
				true	->
					timer:sleep(200),
					login(RoleID, Pid, N+1)
			end
	end.

%% 每次unregister完,都将对应的ets表项删除,防止服务运行很久的时候ets表过大

%% 添加这个进程字典内容是因为，玩儿家进行pvp，则玩儿家id被lock，然后进行战斗计算，若在战斗计算过程中出错，未返回消息给pvp_server
%% 玩儿家将被一直lock，无法继续参加战斗。添加pvpsign，当玩家下线或断线等任一情况发生，role_server都会调用unlock接口清除状态
add_pvp_sign(RoleID, TarRoleID)->
	erlang:put({pvp_sign,RoleID}, TarRoleID).

clear_pvp_sign(RoleID)->
	pvp_server:unexpecte_unlock(RoleID),
	erlang:erase({pvp_sign, RoleID}).

logoff_without_flush(RoleID) when is_integer(RoleID)	->
    erlang:unregister(role_lib:regName(RoleID)),
	role_lib:leave_online_table(RoleID),
	clear_pvp_sign(RoleID),
%% 	plunder_server:role_logoff(RoleID),
	catch hist_server:clear_hist(RoleID),
	ets:delete(?REG_JUDG_TABLE,RoleID);
logoff_without_flush(State) ->
	?ERR("role logout when state was:~w",[State]),
	ok.

logoff_with_flush(RoleID,DeviceID)	when is_integer(RoleID)->
    erlang:unregister(role_lib:regName(RoleID)),
	?CATCH(do_terminate(RoleID)),
	%% 异常保护,防止因为上面某流程失败导致死锁
	clear_pvp_sign(RoleID),
%% 	plunder_server:role_logoff(RoleID),
	catch hist_server:clear_hist(RoleID),
    catch erlang:send(cross_server, {role_offline, RoleID}),
    catch erlang:send(race_server, {role_offline, RoleID}),
    catch erlang:send(race2_server, {role_offline, RoleID}),
    catch erlang:send(alien_server, {role_offline, RoleID}),
    catch role_server:notice_family_online(false),
	catch homestead_server:homestead_role_offline(RoleID),
	catch enargy_server:enargy_role_offline(RoleID),
	catch etc_server:role_offline(RoleID, DeviceID),
	ets:delete(?REG_JUDG_TABLE,RoleID);
logoff_with_flush(State,_) ->
	?ERR("role logout when state was:~w",[State]),
	ok.


do_terminate(RoleID) ->
    role_carlos:cs_carlos_relic_sign_cancel([]),
    erlang:send(relic_server, {role_offline,RoleID}),
    role_lib:leave_online_table(RoleID),
    %% 等待10ms，接受并处理最后的协议
    timer:sleep(10),
    role_server:flush_msg(RoleID),
    %% 持久化
    case is_stop_server() of
        false ->
            ?CATCH(role_persist:persist_all_data());
        true ->
            check_dump_num(RoleID, util:now()),
            ?CATCH(role_persist:persist_all_data()),
            ets:delete(?ETS_ROLE_DUMP,RoleID)
    end,
    ok.

is_stop_server() ->
    case catch ets:lookup(?ETS_STOP_FLAG, flag) of
        [] ->
            false;
        _ ->
            true
    end.

check_dump_num(RoleID, Timestamp) ->
    case erlang:length(ets:tab2list(?ETS_ROLE_DUMP)) < ?DUMP_NUM
         orelse util:now() - Timestamp >= ?DUMP_WAIT_TIME_OUT of
        true  -> 
            ets:insert(?ETS_ROLE_DUMP, {RoleID, 0});
        false ->
            timer:sleep(200),
            check_dump_num(RoleID, Timestamp)
    end.
