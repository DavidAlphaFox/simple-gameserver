%% @author caohongyang
%% @doc 礼包
%% Created 2013-7-5


-module(role_gift).
-compile(export_all).
-include("def_role.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================
cs_gift_request(#cs_gift_request{code=Code}) ->
	#role{roleID=RoleID, accid=Accid} = Role = role_data:get_roleInfo(),
	case check_request_gift(RoleID, Code, Accid) of
		{true, Reward, Type} ->
			?DEBUG("r=~100p",[Reward]),
			db_sql:add_role_gift_drawed_type(RoleID, Type),
			SellReward = setelement(1, Reward, sell_reward),
			role_reward:handle_sell_reward_f(Role, SellReward, ?MONEY_ADD_TYPE_GIFT_CODE, 0, Type),
			RewardInfo = activity_server:sell_reward2p_reward_info(SellReward),
			?sendself(#sc_gift_request{result=1, rewardInfo=[RewardInfo]});
		{false, Reason} ->
			?DEBUG("reason=~w",[Reason]),
			?sendself(#sc_gift_request{result=Reason,rewardInfo=[]})
	end.
			
%% ====================================================================
%% Internal functions
%% ====================================================================

check_request_gift(RoleID, Code, Accid) ->
	Len = length(Code),
	if Len == 10 ->
		   {Type,_} = lists:split(2, Code),
		   case db_sql:check_role_gift_drawed_type(RoleID, Type) of
			   true ->
				   {false, 4};
			   false ->
				   request_platform(Accid, Code, Type)
		   end;
	   Len == 11 ->
		   {Type,_} = lists:split(3, Code),
		   case db_sql:check_role_gift_drawed_type(RoleID, Type) of
			   true ->
				   {false, 4};
			   false ->
				   request_platform(Accid, Code, Type)
		   end;
	   true ->
			if length(Code) == 9 ->
				   {Type,_} = lists:split(2, Code),
				   case db_sql:check_role_gift_drawed_type(RoleID, Type) of
					   true ->
						   {false, 4};
					   false ->
						   request_platform(Accid, Code, Type)
				   end;
			   true ->
				   {false, 3}
			end
	end.

get_value(Response, Key) when is_binary(Key)->
	case lists:keyfind(Key, 1, Response) of
		false ->
			false;
		{Key, Value} ->
			Value
	end.

request_platform(Accid, Code, Type) ->
	ServerID = data_setting:get(server_id),
	ServerKey = data_setting:get(account_server_connect_key),
	AccountServerAddr = data_setting:get(account_server_url),
	Arg = lists:flatten(io_lib:format("accountID=~w&giftcode=~s&serverID=~w&serverKey=~s", 
									  [Accid,Code,ServerID, ServerKey])),
	Response = httpc:request(post, {AccountServerAddr++"/gift",
							  [], "application/x-www-form-urlencoded",Arg}, [], []),
	case Response of
		{ok, {_,_,Content}} ->
			{Content2} = ejson:decode(Content),
			Result = get_value(Content2, <<"result">>),
			case Result of
				1 ->
				   Reward = get_value(Content2, <<"reward">>),
				   case catch binary_to_term(base64:decode(Reward)) of					   
					   #data_gift_reward{} = Reward2 ->
						   {true, Reward2, Type};
					   Other ->
						   ?ERR("request_gift_err:~w",[Other]),
						   {false, 5}
				   end;
			   Result when is_integer(Result)->
				   {false,Result};
			   _ ->
			   		?ERR("request_gift_err2:~w",[Result]),
				   {false, 5}
			end;
		_ ->
			{false, 6}
	end.

%% request_platform(Accid, Code, Type) ->
%%     ServerID = data_setting:get(server_id),
%%     ServerKey = data_setting:get(account_server_connect_key),
%%     case catch gen_server:call({global, util:get_platform_server()}, {gift, ServerID, ServerKey, Code, Accid}) of
%%         {true, Reward} ->
%%             case erlang:is_record(Reward, data_gift_reward) of
%%                 true ->
%%                     {true, Reward, Type};
%%                 false ->
%%                     {false, 5}
%%             end;
%%         {false, Result} ->
%%             if
%%                 Result == 2 ->
%%                     {false, 2};
%%                 Result == 3->
%%                     {false, 3};
%%                 Result == 7->
%%                     {false, 7};
%%                 Result == 8->
%%                     {false, 8};
%%                 Result == 9->
%%                     {false, 9};
%%                 true ->
%%                     ?ERR("request_gift_err2:~w",[Result]),
%%                     {false, 5}
%%             end;
%%         Error ->
%%             ?ERR("Error:~w", [Error]),
%%             {false, 6}
%%     end.
%% 	

			
	
