%% @author lixinglong
%% @doc 充值gold增加记录


-module(behavior_gold_pay_add).
-include("def_behavior.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 write/1
		,log/14
		]).



%% ====================================================================
%% Internal functions
%% ====================================================================

write(State) ->
	db_sql:log_gold_pay_add(State),
	{ok,[]}.

log(RoleID, VipLevel, PayGold, CurGold, PayBonus, GoldBonus, Type, AppItemID, Md5,Accid,DeviceID,SrcType,NChapterID,Level) ->
	{Date, _} = Time = erlang:localtime(),
	erlang:send(?MODULE, {add, {RoleID, VipLevel, PayGold, CurGold, PayBonus,GoldBonus, Date, Time, Type, AppItemID, "", Md5,Accid,DeviceID,SrcType,NChapterID,Level}}).