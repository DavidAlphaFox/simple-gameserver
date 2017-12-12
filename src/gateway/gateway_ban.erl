%% @author caohongyang
%% @doc 帐号封禁相关
%% Created 2013-2-20


-module(gateway_ban).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-compile(export_all).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, check_ban/1]).


%% @初始化
init() ->
	ok.


%% @验证是否被封禁
check_ban(AccountID) ->
	db_sql:check_account_ban(AccountID).

%% @封禁某帐号
ban_by_name(Name) ->
	case db_sql:search_roleName2(util:latin1(Name)) of
		[AccountID, RoleID] when is_integer(AccountID) ->
			case ?CATCH(db_sql:add_account_ban(AccountID)) of
				{ok,_} ->
					?unicast(RoleID, #sc_account_kick{reason=2}),
					timer:sleep(200),
					catch(role_server:stop(RoleID)),
					ok;
				_ ->
					{false, 3}
			end;
		_ ->
			%% 玩家不存在
			{false, 2}
	end.

ban_by_roleID(RoleID) ->
	case db_sql:get_roleInfo(RoleID) of
		#role{accid=AccountID} ->
			case ?CATCH(db_sql:add_account_ban(AccountID)) of
				{ok,_} ->
					ok;
				_ ->
					{false, 3} %% 帐号已被封
			end;
		_ ->
			%% 玩家不存在
			{false, 2}
	end.

ban_by_accountID(AccountID) ->
	?CATCH(db_sql:add_account_ban(AccountID)).

%% @doc 解禁某帐号
free_by_name(Name) ->
	case db_sql:search_roleName2(util:latin1(Name)) of
		[AccountID, _RoleID]  when is_integer(AccountID) ->
			db_sql:del_account_ban([AccountID]),
			ok;
		_ ->
			%% 玩家不存在
			{false, 2}
	end.

free_by_roleID(RoleID) ->
	case db_sql:get_roleInfo(RoleID) of
		#role{accid=AccountID} ->
			db_sql:del_account_ban([AccountID]),
			ok;
		_ ->
			%% 玩家不存在
			{false, 2}
	end.
			
free_by_accountID(AccountID) ->
	db_sql:del_account_ban([AccountID]).


