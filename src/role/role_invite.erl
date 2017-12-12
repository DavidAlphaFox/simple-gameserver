%% @author caohongyang
%% @doc 邀请码、微博相关功能
%% Created 2013-6-6


-module(role_invite).
-compile(export_all).
-include("def_role.hrl").
-include("def_mail.hrl").
-define(MAX_INVITER_SEEN_NUM, 100).
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

%% 非玩家进程通知玩家进程增加微博可分享事件 
add_weibo_share_mark(RoleID, Type) ->
	role_lib:send_server(RoleID, {add_weibo_share_mark, Type}).

%% 标记一个可以分享的事件
do_add_weibo_share_mark(Type) ->
	List = role_data:get_weibo_share_list(),
	case lists:member(Type, List) of
		true ->
			ignore;
		false ->
			role_data:set_weibo_share_list([Type|List])
	end.



cs_invite_info(_) ->
	RoleID = role_data:get_roleID(),
	#d_invite{rewardNum=RewardNum,inviteRoleIDList=List} = invite_server:get_invite(RoleID),
	#limitInfo{inviteRoleID=InviteRoleID,isBindWeibo=IsBindWeibo,inviteRoleName=InviteRoleName} = role_data:get_limitInfo(),
	if is_integer(InviteRoleID), InviteRoleID>0 ->
		   IsInput = true;
	   true ->
		   IsInput = false
	end,
	?sendself(#sc_invite_info{inviteNum=length(List),isBindWeibo=IsBindWeibo,getFirstPayRewardNum=RewardNum,whoInviteYou=InviteRoleName,isInputInviteCode=IsInput}).

cs_invite_bind_weibo(_) ->
	#limitInfo{isBindWeibo=IsBindWeibo} = LimitInfo = role_data:get_limitInfo(),
	if IsBindWeibo == true ->
		   ?sendself(#sc_invite_bind_weibo{result=2});
	   true ->
		   LimitInfo2 = LimitInfo#limitInfo{isBindWeibo=true},
		   role_data:set_limitInfo(LimitInfo2),
		   RoleID = role_data:get_roleID(),
		   Reward = data_invite:get(bind_weibo_reward),
		   mail_server:send_sys_mail(RoleID, ?MAIL_BIND_WEIBO_SUCC, [], "", Reward),
		   ?sendself(#sc_invite_bind_weibo{result=1})
	end.

cs_invite_weibo_share_levelup(#cs_invite_weibo_share_levelup{level=ShareLevel}) ->
	#role{level= RoleLevel} = Role = role_data:get_roleInfo(),
	Num = role_data:get_weibo_count(),
	if Num > 0 ->
			role_data:dec_weibo_count(),
			if ShareLevel >= 10000 ->
				   Type = ShareLevel,
				   List = role_data:get_weibo_share_list(),
				   case lists:member(Type, List) of
					   true ->
						   	List2 = lists:delete(Type, List),
						   	role_data:set_weibo_share_list(List2),
						  	{Base,Ratio} = data_invite:get(weibo_share_arg),
						  	RewardCoin = erlang:trunc(RoleLevel*Ratio+Base),
						  	role_lib:add_coin_f(Role, RewardCoin, ?MONEY_ADD_TYPE_WEIBO_SHARE, ShareLevel, ""),
						   	?sendself(#sc_invite_weibo_share_levelup{result=1});
					   false ->
						   ?sendself(#sc_invite_weibo_share_levelup{result=3})
				   end;
			   true ->
				   #limitInfo{lastShareLevel=LastShareLevel} = LimitInfo = role_data:get_limitInfo(),
				   if RoleLevel >= ShareLevel andalso ShareLevel > LastShareLevel ->
						  LimitInfo2 = LimitInfo#limitInfo{lastShareLevel=ShareLevel},
						  role_data:set_limitInfo(LimitInfo2),
						  {Base,Ratio} = data_invite:get(weibo_share_arg),
						  RewardCoin = erlang:trunc(ShareLevel*Ratio+Base),
						  role_lib:add_coin_f(Role, RewardCoin, ?MONEY_ADD_TYPE_WEIBO_SHARE, ShareLevel, ""),
						  ?sendself(#sc_invite_weibo_share_levelup{result=1});
					  true ->
						  ?sendself(#sc_invite_weibo_share_levelup{result=2})
				   end
			end;
		true ->
			?sendself(#sc_invite_weibo_share_levelup{result=4})
		end.

cs_invite_input_invite_code(#cs_invite_input_invite_code{inviteCode=Input}) ->
	case check_input_invite_code(Input) of
		{true, LimitInfo, InputRoleID, InputRoleName, Role}->
			LimitInfo2 = LimitInfo#limitInfo{inviteRoleID=InputRoleID,inviteRoleName=InputRoleName},
			role_data:set_limitInfo(LimitInfo2),
			Reward = data_invite:get(bind_invite_code_reward),
			role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_INPUT_INVITE_CODE, 0, Input),
			invite_server:input_invite_code(Role#role.roleID, InputRoleID),
			?sendself(#sc_invite_input_invite_code{result=1,inviterName=InputRoleName});												  
		{false, Reason}->
			?sendself(#sc_invite_input_invite_code{result=Reason,inviterName=""})
			end.

cs_invite_list(_) ->
	RoleID = role_data:get_roleID(),
	#d_invite{inviteRoleIDList=IDList} = invite_server:get_invite(RoleID),
	InfoList = invite_list(lists:sublist(IDList,?MAX_INVITER_SEEN_NUM), []),
	?sendself(#sc_invite_list{inviteList=InfoList}).

invite_list([ID|IDList], Result) ->
	case role_lib:get_rolePublic(ID) of
		[] ->
			invite_list(IDList, Result);
		RolePublic ->
			E = rolePublic2p_invite(RolePublic),
			invite_list(IDList, [E|Result])
	end;
invite_list([], Result) ->
	Result.

check_input_invite_code(Input) ->
	if length(Input) =/= 6 ->
		   {false, 2};
	   true ->
		   #limitInfo{inviteRoleID=InviteRoleID} = LimitInfo = role_data:get_limitInfo(),
		   if is_integer(InviteRoleID) andalso InviteRoleID > 0 ->
				  {false,4};
			  true ->
				  case catch tk_id:inviteCode2roleID(Input) of
					  InputRoleID when is_integer(InputRoleID) ->
						  Role = role_data:get_roleInfo(),
						  if Role#role.roleID =:= InputRoleID ->
								 {false, 5};
							 true ->
								 case role_lib:get_name(InputRoleID) of
									 "" ->
										 {false, 3};
									 InputRoleName ->
										 {true, LimitInfo, InputRoleID, InputRoleName,Role}
								 end
						  end;
					  _ ->
						  {false, 3}
				  end
		   end
	end.
				   
		

	
			
%% ====================================================================
%% Internal functions
%% ====================================================================


rolePublic2p_invite(RolePublic) ->
	if RolePublic#rolePublic.goldTotalPaid >0 ->
		   IsPay = true;
	   true ->
		   IsPay = false
	end,
	#p_invite{isPay=IsPay,
			  title=RolePublic#rolePublic.title,
			  roleName=RolePublic#rolePublic.roleName,
			  roleID=RolePublic#rolePublic.roleID,
			  level=RolePublic#rolePublic.level,
			  isMale=RolePublic#rolePublic.isMale}.



