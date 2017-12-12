%% @author:lixinglong
%% @doc:send reward to anyone by mail

-module(role_mail_gift).
-include("common.hrl").
-include("def_mail.hrl").
-compile(export_all).
-export([send_gift/2, send_gift/5, send_gift_c/3,send_everyone_reward/4,send_gift_by_name/3,send_onliners_gift/2]).
-define(LISTLEN, 50).
%% vip设置结构
-record(vip_info,{roleid=0,vip=0}).

%% 防止一次性发送过多邮件导致mail_server负载过大，内存开销过大。
%% 去除机器人id 并判断id有效性

%% 给所有玩家发送邮件礼包
send_everyone_reward(Reward, Text, MailID, MailArgs) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_all_roles(),
	send_gift(RoleList, Reward, Text2,  MailID, MailArgs).

%% 给除MaskList外的所有人发邮件礼包 
send_everyone_reward_except(Reward, Text, MailID, MailArgs, MaskList) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_all_roles() -- MaskList,
	send_gift(RoleList, Reward, Text2,  MailID, MailArgs).

send_type_level_and_vip_role_reward(Reward, Text, LevelLow,LevelHigh,VipLow,VipHigh,Type,OffTime,OffTime2,MailID, MailArgs) ->
	Text2 = util:latin1(Text),
	RoleList = get_type_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh,Type,OffTime,OffTime2),
	send_gift(RoleList, Reward, Text2, MailID, MailArgs).

get_type_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh,Type,OffTime,OffTime2) when Type =:= -1 ->
    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime <= ~w and lastLogoutTime > ~w;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,VipHigh,OffTime,OffTime2]),
    case db_sql:get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end;
get_type_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh,Type,OffTime,OffTime2) ->
    Sql = io_lib:format("select roleID from gRole where roleID >= ~w and level >= ~w and level < ~w and vipLevel >= ~w and vipLevel < ~w and lastLogoutTime <= ~w and lastLogoutTime > ~w and srcType = ~w;",
                           [tk_id:robot_roleID_max(),LevelLow,LevelHigh,VipLow,VipHigh,OffTime,OffTime2,Type]),
    case db_sql:get_all(Sql) of
        [_|_] = List ->
            [E||[E]<-List];
        _ ->
            []
    end.


%% 给指定等级范围的玩儿家发邮件礼包
send_level_reward(Reward, Text, LevelLow, LevelHigh) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_level_roles(LevelLow, LevelHigh),
	send_gift(RoleList, Reward, Text2, 0, []).

%% 给指定等级范围的玩家发邮件礼包
send_level_reward(Reward,Text,LevelLow,LevelHigh,MailID,MailArgs) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_level_roles(LevelLow, LevelHigh),
	send_gift(RoleList, Reward, Text2, MailID, MailArgs).

%% 给指定等级范围，登录时间范围的玩儿家发邮件礼包
send_level_offtime_reward(Reward, Text, LevelLow, LevelHigh, OfftimeLow, OfftimeHigh, MailID, MailArgs) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_level_offtime_roles(LevelLow, LevelHigh, OfftimeLow, OfftimeHigh),
	send_gift(RoleList, Reward, Text2, MailID, MailArgs).

%% 给指定vip范围的玩家发送邮件礼包
send_vip_reward(Reward, Text, VipLow, VipHigh)->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_vip_roles(VipLow, VipHigh),
	send_gift(RoleList, Reward, Text2, 0, []).

%% 给指定vip范围，登录时间范围的玩儿家发邮件礼包
send_vip_offtime_reward(Reward, Text, VipLow, VipHigh, OfftimeLow, OfftimeHigh) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_vip_offtime_roles(VipLow, VipHigh, OfftimeLow, OfftimeHigh),
	send_gift(RoleList, Reward, Text2, 0, []).

%% 给指定等级及vip范围的玩家发送邮件礼包
send_level_vip_reward(Reward, Text, LevelLow, LevelHigh, VipLow, VipHigh) ->
    Text2 = util:latin1(Text),
    RoleList = db_sql:get_level_and_vip_roles(LevelLow,LevelHigh,VipLow,VipHigh),
    send_gift(RoleList, Reward, Text2, 0, []).

%% 给指定等级及vip范围及最后登录时间的玩家发送邮件礼包
send_level_vip_offtime_reward(Reward, Text, LevelLow, LevelHigh, VipLow, VipHigh, OfftimeLow, OfftimeHigh) ->
	Text2 = util:latin1(Text),
	RoleList = db_sql:get_level_and_vip_and_offtime_roles(LevelLow,LevelHigh,VipLow,VipHigh,OfftimeLow,OfftimeHigh),
	send_gift(RoleList, Reward, Text2, 0, []).

%% 给所有在线玩家发送礼包
send_onliners_gift(Reward, Text) ->
	Text2 = util:latin1(Text),
	RoleList = lists:foldl(fun({E,_}, Acc)->
								   [E|Acc]
						   end, [], ets:tab2list(?ETS_ROLE_ONLINE)),
	send_gift(RoleList, Reward, Text2, 0, []).

%%在控制台通过角色id发送邮件礼包
send_gift_c(RoleList, Reward,Text) ->
	Text2 = util:latin1(Text),
	send_gift(RoleList, Reward, Text2, 0, []).

%% 在控制台通过名字发送邮件礼包
send_gift_nc(RoleList, Reward,Text) ->
	Text2 = util:latin1(Text),
	send_gift_by_name(RoleList, Reward, Text2).

%% 发送邮件礼包
send_gift(RoleList, Reward)->
	send_gift(RoleList, Reward, "", 0, []).


send_gift(RoleList, Reward, Text, MailID, MailArgs)->
	?INFO("start to mail gift:~w",[RoleList]),
	send_gift2(RoleList, Reward, Text, MailID, MailArgs),
	?INFO("end to mail gift").

%%通过名字列表发送邮件礼包
send_gift_by_name(NameList, Reward, Text)	->
	lists:foreach(fun(E)-> send_gift_n(E, Reward, Text)
						  end
						  , NameList).

%% 通过名字发送邮件礼包
send_gift_n(Name, Reward, Text)->
	case db_sql:search_roleName2(util:latin1(Name)) of
		[AccountID, RoleID]  when is_integer(AccountID) ->
			send_gift([RoleID], Reward, Text, 0, []);
		_ ->
			?ERR("role name not exist:~w",[Name]),
			false
	end.

%% 使用玩家列表发送邮件礼包的功能函数
send_gift2(RoleList, Reward, Text, MailID, MailArgs)	->	
    RL=[E|| E <- RoleList ,not(tk_id:is_robot(E)), valid_roleID(E)],
    case MailID of
        0 ->
            NewMailID = ?MAIL_OTHER_REWARD;
        _ ->
            NewMailID = MailID
    end,
    mail_server:send_role_id_list_sys_mail(RL, NewMailID, MailArgs, Text, Reward).

%% 判决角色ID是否有效
valid_roleID(RoleID)->
	case db_sql:get_roleInfo(RoleID) of
		?undefined ->
			?ERR("role id not exist:~w",[RoleID]),
			false;
		_ ->
			true
	end.
set_role_vip(VipList)->
	F = 
	fun(#vip_info{roleid=RoleID,vip=Vip})->
		case catch role_lib:send_server(RoleID, {set_role_vip,Vip}) of
			{'EXIT',_}->
				db_sql:update_role_vip(RoleID,Vip);
			_ ->
				ignore							  
		end
	end,
	lists:foreach(fun(E)->spawn(fun()-> F(E) end) end ,VipList).
%%---------------------------------------------------------------------------------------------------------
test()->
	role_mail_gift:send_gift([3000147,3000146,3000144,3000142,3003261,200002, 3000145,3003263], 
							 {sell_reward,300000,0,0,0,[{new_item,24103,10,1,0}],0,[]},
							 "{prop ffff0000-14}虎牢关-离线参与奖励/n{prop ffff5050-12}恭喜主公离线参与虎牢关活动获得奖励！").
test2()->
	role_mail_gift:send_gift_by_name(["孔文莹"], 
							 {sell_reward,300000,0,0,0,[{new_item,24103,10,1,0}],0,[]},
							 "{prop ffff0000-14}虎牢关-离线参与奖励/n{prop ffff5050-12}恭喜主公离线参与虎牢关活动获得奖励！").
test3()->
	%role_mail_gift:send_gift_nc(["失态啊师太"], {sell_reward,1000000,0,0,30000,[{new_item,24103,10,1,0}],0,[]},""),
	role_mail_gift:send_everyone_reward({sell_reward,300000,0,0,30000,[{new_item,21110,100,1,0},{new_item,24103,10,1,0}],80000,[]},"邮件恢复/n大家周末省着点用/n", 0, []).





	
