%% @author 李兴龙
%% @doc 聊天处理
%% Created 2013-11-13


-module(role_talk).
-compile(export_all).
-include("def_role.hrl").

%% API functions
-export([]).

%% Internal functions
-export([]).

-define(last_talk_time,last_talk_time).
-define(last_family_talk_time,last_family_talk_time).

%% ====================================================================
%% API functions
%% ====================================================================

%% 2015-11-2
%% 杨攀(杨攀) 11:47:18
%% 解除屏蔽之后  只能看到屏蔽之前的消息 和解除屏蔽之后发的消息

cs_talk_world(#cs_talk_world{talkMessage=Message, channel=Channel})->
	case check_talk(Message, Channel) of
		{talk_world,Role,NeedGold,Now}->
			do_talk_world(Now,Message,Role,NeedGold);
        {talk_family, Now, FamilyID, Role} ->
            do_talk_family(Now,Message,Role,FamilyID);
		{false, Reason}->
%%             ?ERR("Reason:~w", [Reason]),
			?sendself(#sc_talk_world{result=Reason, channel=Channel})
	end.
	
cs_talk_gag_one(#cs_talk_gag_one{roleName=RoleName})->
	GagList = role_data:get_gag_list(),
    case lists:keytake(RoleName, #gag_info.roleName, GagList) of
        {value,GagInfo,OtherList} ->
            OldStList = GagInfo#gag_info.timeStamp,
            L = erlang:length(OldStList),
            if
                (L rem 2) =:= 0 ->
                    NewGagInfo = GagInfo#gag_info{timeStamp=lists:reverse([util:now()|lists:reverse(OldStList)])},
                    role_data:set_gag_list([NewGagInfo|OtherList]);
                true -> %已经处于屏蔽中了
                    ignore
            end;
        false ->
            role_data:set_gag_list([{gag_info,RoleName,[util:now()]}|GagList])
    end.

cs_talk_ungag_one(#cs_talk_ungag_one{roleName=RoleName})->
	GagList = role_data:get_gag_list(),
    case lists:keytake(RoleName, #gag_info.roleName, GagList) of
        {value,GagInfo,OtherList} ->
            OldStList = GagInfo#gag_info.timeStamp,
            L = erlang:length(OldStList),
            if
                (L rem 2) =:= 1 ->
                    NewGagInfo = GagInfo#gag_info{timeStamp=lists:reverse([util:now()|lists:reverse(OldStList)])},
                    role_data:set_gag_list([NewGagInfo|OtherList]);
                true -> %处于非屏蔽中
                    ignore
            end;
        false -> %处于非屏蔽中
            ignore
    end.


cs_talk_get_gag_list(_)->
	?sendself(#sc_talk_get_gag_list{gag_info_list=role_data:get_gag_list()}).

cs_talk_send_whisper(#cs_talk_send_whisper{roleID=TarRoleID,talkMessage=TalkMessage0})->
	TalkMessage = util:words_filter(TalkMessage0),
    SendRoleID = role_data:get_roleID(),
    Length = length(TalkMessage),
    if
        Length =:= 0 ->
            ?sendself(#sc_talk_send_whisper{result=2});
        SendRoleID =:= TarRoleID ->
            ?sendself(#sc_talk_send_whisper{result=3});
        true ->
            case role_lib:is_online(TarRoleID) of
                true ->
                    case catch role_lib:send_server(TarRoleID, {cs_talk_send_whisper, SendRoleID,TalkMessage}) of
                        {cs_talk_send_whisper, SendRoleID,TalkMessage}->
                            next;
                        _ ->
                            db_sql:add_whisper(TarRoleID,SendRoleID,TalkMessage)
                    end;
                false ->
                    case db_sql:if_role_exist(TarRoleID) of
                        true ->
                            db_sql:add_whisper(TarRoleID,SendRoleID,TalkMessage);
                        false ->
                            ignore
                    end
            end,
            ?sendself(#sc_talk_send_whisper{result=1})
    end.
cs_talk_get_whisper(_)->
    HistoryList = case get(?whisper_list) of
        WhisperList when is_list(WhisperList)->
            WhisperList;
        _ ->
           []
    end,
    erase(?whisper_list),
    ?sendself(#sc_talk_get_whisper{record_list=HistoryList++get_whisper_all(role_data:get_roleID())}).

%% ====================================================================
%% Internal functions
%% ====================================================================

check_talk(Message, ?CHAT_CHANNEL_WORLD) ->
	case erlang:length(Message) =< data_talk:get(talk_words_limit) * 3 of
		true ->
			Now=util:now(),
			case Now - get_last_talk_time() > data_talk:get(talk_interval_limit) of
				true ->
					#role{level=RoleLevel,vipLevel=VIPLevel}=Role = role_data:get_roleInfo(),
					NeedGold = 
						case  VIPLevel >= data_talk:get(free_vipLevel) of
							true ->
							   0;
						   false ->
							   data_talk:get(talk_need_gold)
						end,
					case RoleLevel >= data_talk:get(talk_need_level) of
						true ->
							case role_lib:check_money(Role, gold, NeedGold) of
								true ->
									case check_role_gag() of
										false ->
											{talk_world, Role, NeedGold,Now};
										true ->
											{false, 6}
									end;
								false ->
									{false, 2}
							end;
						_ ->
							{false, 5}
					end;
				_ ->
					{false,3}
			end;
		_ ->
			{false,4}
	end;
check_talk(Message, ?CHAT_CHANNEL_FAMILY) ->
    #role{familyID=FamilyID} = Role = role_data:get_roleInfo(),
    case erlang:length(Message) =< data_family:get(talk_words_limit) * 3 of
        true ->
            case FamilyID > 0 of
                true ->
                    Now = util:now(),
                    case get_last_family_talk_time() + data_family:get(talk_interval_limit) < Now of
                        true ->
                            {talk_family, Now, FamilyID, Role};
                        false ->
                            {false, 8}
                    end;
                false ->
                    {false, 7}     
            end;
        false ->
            {false, 4}
    end.

check_role_gag()->
	case talk_server:is_gaged(role_data:get_roleID()) of
		true ->
			true;
		_ ->
			false
	end.


do_talk_world(Now,Message,Role,NeedGold)->
    role_lib:deduct_gold_2_f(Role, NeedGold, ?MONEY_DEC_TYPE_WORLD_TALK, 0, ""),
    
    send_every_one_message(util:words_filter(Message),Now,Role),
    
    set_last_talk_time(Now),
	?CATCH(role_task_trigger:handle({dispach_task,role_chat_times})),
    ?sendself(#sc_talk_world{result=1, channel=?CHAT_CHANNEL_WORLD}).


do_talk_family(Now,Message,Role,FamilyID)->
    send_family_message(FamilyID, util:words_filter(Message),Now,Role),
    set_last_family_talk_time(Now),
	?CATCH(role_task_trigger:handle({dispach_task,role_chat_times})),
    ?sendself(#sc_talk_world{result=1, channel=?CHAT_CHANNEL_FAMILY}).

get_last_talk_time()->
	case erlang:get(?last_talk_time) of
		?undefined ->
			0;
		X ->
			X
	end.

set_last_talk_time(Time) ->
	erlang:put(?last_talk_time,Time).

get_last_family_talk_time()->
    case erlang:get(?last_family_talk_time) of
        ?undefined ->
            0;
        X ->
            X
    end.

set_last_family_talk_time(Time) ->
    erlang:put(?last_family_talk_time,Time).

send_every_one_message(Message,Now,#role{title=Title,roleName=Name,roleID=RoleID,location=Location,head=Head,isMale=IsMale,vipLevel=VipL,level = Level})->
	Data = #sc_talk_world_message{channel=?CHAT_CHANNEL_WORLD,roleName=Name,roleTitle=Title,message=Message,timeStamp=Now
                                 ,roleID=RoleID,familyTitle=0,location=Location,head=Head,is_male=IsMale,vip_level=VipL
								 ,grade=doublematch_server:dirty_get_role_dm_grade(RoleID),level = Level},
	broadcast_server:bc(Data),
%%     ?ERR("Data:~w", [Data]),
	erlang:send(talk_server, {talk_channel_world, Data}).

send_family_message(FamilyID, Message,Now,#role{title=Title,roleName=Name,roleID=RoleID,location=Location,head=Head,isMale=IsMale,vipLevel=VipL,level = Level})->
    Data = #sc_talk_world_message{channel=?CHAT_CHANNEL_FAMILY,roleName=Name,roleTitle=Title,message=Message,timeStamp=Now
                                 ,roleID=RoleID,familyTitle=0,location=Location,head=Head,is_male=IsMale,vip_level=VipL,
								  grade = doublematch_server:dirty_get_role_dm_grade(RoleID),level = Level},
    family_misc:router_to_family_process(FamilyID, {bc, Data}),
%%     ?ERR("Data:~w", [Data]),
    erlang:send(talk_server, {talk_channel_family, Data}).

%% 获取后会删除数据库中的离线记录
get_whisper_all(RoleID)->
    db_sql:get_whisper(RoleID).

is_gag_role(GagInfoList,RoleName) when is_list(GagInfoList)->
    case lists:keyfind(RoleName, #gag_info.roleName, GagInfoList) of
        false->
            false;
        FindGagInfo->
            OldStList = FindGagInfo#gag_info.timeStamp,
            L = erlang:length(OldStList),
            if
                (L rem 2) =:= 1 ->
                    true;
                true -> %处于非屏蔽中
                    false
            end
    end;
is_gag_role(_GagInfoList,_RoleName)->
    false.

load_words([{Tag,Data}]) ->
	[{Tag,[unicode:characters_to_binary(D)||D<-Data]}].




