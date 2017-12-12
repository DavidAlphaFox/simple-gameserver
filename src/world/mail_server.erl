%% 数据以数据库为基准，内存中做了部分数据的cache
%% 标记已读：将未读邮件发送给玩家后，该邮件被标记为已读，并立即将标记操作更新到数据库
%% 新增邮件：将邮件添加进进程字典的新增邮件列表，每隔dumptick将进程字典同步到数据库，给玩儿家发送新邮件通知是在数据同步到数据库之后
%% 删除邮件：将邮件添加进进程字典的删除邮件列表，每隔dumptick将进程字典同步到数据库，玩儿家删除邮件与领取奖励都会先到删除列表中确认该邮件是否已删除，
%%			 如果不是已删除邮件，则到数据库中查询是否存在该邮件，存在则继续走删除或领奖流程。邮件删除后需要把缓存中该邮件一同删除。
%% 缓存邮件：玩儿家初始化登录时，从sql取得GET_SQL_MAIL_NUM条邮件，将其中MAIL_NUM_NODE_NUM条发送给玩家，余下的存放进缓存，当玩家获取更多邮件
%%			 服务器再次从sql取得GET_SQL_MAIL_NUM条邮件，并将这些邮件与缓存邮件拼接在一起，将其中MAIL_NUM_NODE_NUM条发送给玩家，余下继续缓存
%%			 通过使用缓存，可以降低sql访问次数。若玩家访问更多时，缓存的数量比MAIL_NUM_NODE_NUM多，则不访问sql，直接发送缓存邮件给玩家。
%% 缓存格式：({RoleID,Type},MailList)
%% 缓存更新：由于新邮件的访问走的是info流程，该流程会从数据库访问数据，所以新邮件不需要更新cache。但是删除邮件需要更新cache，more流程是从cache中拿数据
%% 发送系统邮件：奖励列表为空时，arg会标记为系统邮件，否则arg标记为奖励邮件。系统邮件与奖励邮件都没发送者的name，发送者的id都为0。
%% tick：每隔dumptick秒，进程字典中数据会刷新到数据库，内容为已删除邮件和新增邮件。每隔tick并检查进程内存，若进程占用内存超过预期，会将每个玩家的缓存都清空
%% 注：若数据库访问会成为瓶颈，可设置GET_SQL_MAIL_NUM为更大值 或 增加topNMail的缓存，这个缓存可用来减少每次info访问造成的io，需要对增加、删除、已读邮件都做好同步操作

-module(mail_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").
-include("def_mail.hrl").
-compile(export_all).
-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([i/0, send_sys_mail/5, send_add_friend_mail/4, send_mail/8, send_notemp_mail/2, send_role_id_list_sys_mail/5]).

-define(DUMP_INTERVAL, 3).%写数据库的间隔,单位：秒
-define(TICK_INTERVAL, 60). % 同步数据库并检查进程buff大小的时间间隔，单位：秒
-define(MAIL_NUM_NODE_NUM, 30).% 一次发送的邮件数量
-define(MAIL_NUM_SYNC_NUM, 40).% 同步刚收到的新邮件时的最大数量
-define(GET_SQL_MAIL_NUM, 200).% the max num of mails get from sql once
-define(MAIL_TYPE_LIST,[1,2,3,4,5,6]). %邮件类型列表
%% ===================Dict Key Begin =========================
%-define(READ_LIST, read_list).%进程字典中缓存起来的等待写数据库的已读标记
-define(DEL_LIST, del_list).%进程字典中缓存起来的等待写入删除记录的列表
-define(ADD_LIST, add_list).%进程字典缓存起来的，新增邮件，等待写入数据库
%% ===================Dict Key End   =========================

-define(send_strange_count, send_strange_count). % 需要固化
-record(send_strange_count,{date={0,0,0}
                           ,count_list=[]}). %% 记录每日玩家向陌生人发送邮件的数量

send_notemp_mail(RoleID, NoTempId)->
	#data_temp_mail{mailInfoList=MailInfoList} = data_temp_mail:get(NoTempId),
	lists:foreach(fun(#mail_template{content=Content, reward=Reward})->
						  send_mail(0,"",RoleID, ?MAIL_TYPE_REWARD, ?MAIL_NONE_TEMPLATE_MAIL,[], Content, Reward)
				  end,MailInfoList).

%% @doc 发送一封系统邮件给某个玩家
send_sys_mail(RoleID, TemplateID, ArgList, Content, Reward) ->
	if Reward =:= [] ->
		   MailType = ?MAIL_TYPE_SYS;	   
	   true ->
		   MailType = ?MAIL_TYPE_REWARD
	end,
	SenderID = 0,
	SenderName = "",
	send_mail(SenderID, SenderName, RoleID, MailType, TemplateID, ArgList, Content, Reward).

%% @doc 给RoleIDList发邮件
send_role_id_list_sys_mail(RoleIDList, TemplateID, ArgList, Content, Reward) ->
    if Reward =:= [] ->
           MailType = ?MAIL_TYPE_SYS;      
       true ->
           MailType = ?MAIL_TYPE_REWARD
    end,
    SenderID = 0,
    SenderName = "",
    erlang:send(?MODULE, {send_role_id_list_mail, SenderID, SenderName, RoleIDList, MailType, TemplateID, ArgList, Content, Reward}).

%% 发送邮件
send_mail(SenderID, SenderName, RoleID, MailType, TemplateID, ArgList, Content, Reward) ->
	erlang:send(?MODULE, {send_mail, SenderID, SenderName, RoleID, MailType, TemplateID, ArgList, Content, Reward}).

%% 发送好友申请邮件
send_add_friend_mail(SenderID, SenderName,RoleLevel, RoleID) ->
    if
        RoleLevel >= 301 ->
	       send_mail(SenderID, "", RoleID, ?MAIL_TYPE_ADD_FRIEND, ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST_TRANSMIGRATION, [SenderName,RoleLevel-300], "", []);
        true ->
           send_mail(SenderID, "", RoleID, ?MAIL_TYPE_ADD_FRIEND, ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST, [SenderName,RoleLevel], "", [])
    end.

%% 发送邀请公会邮件
send_family_invite_mail(SenderID, SenderName, RoleID,FamilyID,FamilyName) ->
    send_mail(SenderID, "", RoleID, ?MAIL_TYPE_INVITE, ?MAIL_TEMPLATE_ADD_FRIEND_INVITE, [SenderName,FamilyName,FamilyID], "", []).

i() ->
	gen_server:call(?MODULE, i).

start() ->
	{ok,_}=
	supervisor:start_child(world_sup, 
						   {?MODULE,
							{?MODULE, start_link, []},
							permanent, 600000, worker, [?MODULE]}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	random:seed(util:gen_random_seed()),
	process_flag(trap_exit,true),
    lists:foreach(fun
                     (Date) when erlang:is_record(Date, send_strange_count)-> 
                          put(?send_strange_count,Date);
                     (_) ->
                          ignore
                  end, db_sql:get_etc(?DB_ETC_KEY_MAIL_SERVER)),
    case get(?send_strange_count) of
        (Date) when erlang:is_record(Date, send_strange_count)-> 
           ignore;
        _ ->
           put(?send_strange_count,#send_strange_count{})
    end,
	tick(),
	dump_tick(),
	{ok, ?undefined}.

handle_call(i, _From, State) ->
	{reply, State, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info(dump_tick, State)->
	?CATCH(do_write_db()),
	dump_tick(),
	{noreply, State, hibernate};

handle_info(tick, State) ->
	
	{memory,Memory }= erlang:process_info(self(),memory),
	
	MemoryByM = Memory div (1024*1024),
	MaxBuffSizeByM = data_setting:get(mail_process_max_buff_size),
	if MemoryByM >= MaxBuffSizeByM ->
		   ?ERR("MAIL_SERVER clear buff TICK...BUFF_SIZE=~wM",[MemoryByM]),
		   %% 清理缓存前，先写数据库
		   case ?CATCH(do_write_db()) of
			   ok ->
				   clear_buff();
			   _ ->
				   ignore
		   end;
	   true ->
		   ignore
	end,
	erlang:garbage_collect(),
	tick(),
	{noreply, State, hibernate};

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
    
handle_info(Info, State) ->
	?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	do_write_db(),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% 客户端初始化邮件，或者第二次打开邮件界面时，请求同步新邮件，并重新生成cache
do_handle_info({client_msg, RoleID, #cs_mail_info{type=Type,clientTopMailUID=ClientTopMailUID}}) ->
	if  Type =:= 1 orelse Type =:= 2 orelse Type =:= 3->
			#d_mail{mail=[ML]} = get_last_n_mail(RoleID, Type,ClientTopMailUID),
			if 	ClientTopMailUID =:= 0 ->
					{SendClientMail, CerMailList2, NewReadList} = init_client_mail(ML),
					sql_sync_read(NewReadList),
					SendClientMail2 = transform_p_mail_list(SendClientMail),
					#d_mail{unreadNum=UN}=update_mail(RoleID, Type, CerMailList2),
					?unicast(RoleID, #sc_mail_info{result=1, isFuckData=false, mailList=SendClientMail2, newMailNum=UN,type=Type});
				true ->
					case ML of
						[] ->
							?unicast(RoleID, #sc_mail_info{isFuckData=true, mailList=[], newMailNum=[], result=3, type=Type});
						[TopM|_] ->
							if 	TopM#mail.mailUID =:= ClientTopMailUID ->
									?unicast(RoleID,#sc_mail_info{isFuckData=false,mailList=[],newMailNum=[],result=3,type=Type});
								true ->
									{SendClientMail, CerMailList2, DoDiscardData, NewReadList}=sync_mail(ML, ClientTopMailUID),
									sql_sync_read(NewReadList),
									SendClientMail2 = transform_p_mail_list(SendClientMail),
									#d_mail{unreadNum=UN} = update_mail(RoleID, Type, CerMailList2),
									?unicast(RoleID, #sc_mail_info{result=1, isFuckData=DoDiscardData, mailList=SendClientMail2, newMailNum=UN, type=Type})
							end
					end
			end;
	true->
		?unicast(RoleID, #sc_mail_info{result=2,type=Type,isFuckData=false,mailList=[],newMailNum=[]})
	end;

do_handle_info({client_msg, RoleID, #cs_mail_more{type=Type, startMailUID=StartMailUID}})->
	if Type =:= 1 orelse Type =:= 2 orelse Type =:= 3->
			ML = get_more_mail(RoleID, Type, StartMailUID),
			if  ML =:= [] ->
					?unicast(RoleID, #sc_mail_more{result=2, type=Type, mailList=[], newMailNum=[]});
				true ->
					{SendClientMail, CerMailList2, NewReadList} = init_client_mail(ML),
					sql_sync_read(NewReadList),
					SendClientMail2 = transform_p_mail_list(SendClientMail),
					#d_mail{unreadNum=UN}=update_mail(RoleID, Type, CerMailList2),
					?unicast(RoleID, #sc_mail_more{result=1,type=Type,mailList=SendClientMail2,newMailNum=UN})
			end;
		true ->
			?unicast(RoleID, #sc_mail_more{result=3,type=Type,mailList=[],newMailNum=[]})
	end;
do_handle_info({client_msg, RoleID, #cs_mail_draw_reward{mailUID=MailUID}}) ->
	case lists:member(MailUID, get_delList()) of
		false ->
			case get_mail_dtl(RoleID, MailUID) of
				false ->
					?unicast(RoleID, #sc_mail_draw_reward{result=2});
				{Reward0, MailTemplateID,_,_}->
                    case check_mail_draw_reward(RoleID,Reward0, MailTemplateID) of
                        {ok,Reward} ->
        					erlang:send(role_lib:regName(RoleID), {draw_mail_reward,Reward,MailTemplateID}),
        					add_delList([MailUID]),
        					update_cache(RoleID, 1, MailUID),
                            ?unicast(RoleID, #sc_mail_draw_reward{result=1}),                            
                            if
                                MailTemplateID =:= ?MAIL_FAMILY_BE_AUTO_DISBAND ->
                                    del_all_family_disband_mail(RoleID);
                                true ->
                                    next
                            end;
                        {fail,Reason} ->
                            ?unicast(RoleID, #sc_mail_draw_reward{result=Reason})
                    end
			end;
		_ ->
			?unicast(RoleID, #sc_mail_delete{result=2})
	end;

do_handle_info({client_msg, RoleID, #cs_mail_delete{mailUID=MailUID, type=Type}})->
    case lists:member(MailUID, get_delList()) of
        false ->
            case get_mail_dtl(RoleID, MailUID) of
                false ->
                    ?unicast(RoleID, #sc_mail_delete{result=2});
                {_,MailTemplateID,MailType,SenderID}->
                    if 	MailType =:= ?MAIL_TYPE_REWARD ->
                            ?unicast(RoleID, #sc_mail_delete{result=3});
                        Type =:= 3 andalso (MailTemplateID /= ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST
                                            andalso MailTemplateID /= ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST_TRANSMIGRATION) ->
                            ?unicast(RoleID, #sc_mail_delete{result=2});
                        true->
                            case MailTemplateID of
                                ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST ->
                                    enargy_server:enargy_remove_friend2(SenderID, RoleID);
                                ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST_TRANSMIGRATION ->
                                    enargy_server:enargy_remove_friend2(SenderID, RoleID);
                                _ ->
                                    next
                            end,
                            add_delList([MailUID]),
                            update_cache(RoleID, Type, MailUID),
                            case MailTemplateID of
                                ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST ->
                                    next;
                                ?MAIL_TEMPLATE_ADD_FRIEND_REQUEST_TRANSMIGRATION ->
                                    next;
                                _ ->
                                    ?unicast(RoleID, #sc_mail_delete{result=1})
                            end
                    end
            end;
        _ ->
            ?unicast(RoleID, #sc_mail_delete{result=2})
    end;

do_handle_info({client_msg, RoleID, #cs_mail_del_spec_mail{senderID=SenderID}}) ->
    case db_sql:sql_find_spec_mailUID_list(RoleID, SenderID, ?MAIL_TYPE_PRIVATE) of
        [] ->
            ?unicast(RoleID, #sc_mail_del_spec_mail{mailUIDList=[]});
        MailUIDList ->
            DelList = get_delList(),
            case lists:filter(fun(MailUID) -> not lists:member(MailUID, DelList) end, MailUIDList) of
                [] ->
                    ?unicast(RoleID, #sc_mail_del_spec_mail{mailUIDList=[]});
                MailUIDList2 ->
                    add_delList(MailUIDList2),
                    update_cache2(RoleID, 2, MailUIDList2),
                    ?unicast(RoleID, #sc_mail_del_spec_mail{mailUIDList=MailUIDList2})
            end
    end;

do_handle_info({role_mail,RoleID,#cs_mail_new{content=Content, targetRoleID=TargetRoleID},Head,IsMale})->
	case TargetRoleID of
		0 ->
			ignore;
		_ ->
            FriendList = friend_server:get_role_friend_roleID(RoleID),
            case lists:member(TargetRoleID, FriendList) of
                true ->
                    SenderName = role_lib:get_name(RoleID),
                    MailUID = tk_id:gen_mailUID(),
                    NowSec = util:now(),
                    Mail = #mail{content=Content,isRead=false,mailReward=[],mailTemplateID=0,mailType=?MAIL_TYPE_PRIVATE,mailUID=MailUID,paramList=[],senderID=RoleID,senderName=SenderName,time=NowSec,head=Head,isMale=IsMale},
                    add_addList(TargetRoleID, Mail),
                    ?unicast(RoleID, #sc_mail_new{result=1});
                false ->
                    case check_strange_count(RoleID) of
                        false ->
                            ?unicast(RoleID, #sc_mail_new{result=7});
                        NewStrangeCount ->
                            put(?send_strange_count,NewStrangeCount),
                			SenderName = role_lib:get_name(RoleID),
                			MailUID = tk_id:gen_mailUID(),
                			NowSec = util:now(),
                			Mail = #mail{content=Content,isRead=false,mailReward=[],mailTemplateID=0,mailType=?MAIL_TYPE_PRIVATE,mailUID=MailUID,paramList=[],senderID=RoleID,senderName=SenderName,time=NowSec,head=Head,isMale=IsMale},
                			add_addList(TargetRoleID, Mail),
                			?unicast(RoleID, #sc_mail_new{result=1})
                    end
            end
	end;

do_handle_info({client_msg, RoleID, #cs_mail_new{content=Content, targetRoleID=TargetRoleID, targetRoleName=TargetRoleName}})->
	TargetRoleID2 = get_targetRoleID(TargetRoleID, TargetRoleName),
	if 	TargetRoleID2 =:= 0 ->
			?unicast(RoleID, #sc_mail_new{result=4});
		length(Content) >= 1000 ->
			?unicast(RoleID, #sc_mail_new{result=2});
		true->
			SenderName = role_lib:get_name(RoleID),
			MailUID = tk_id:gen_mailUID(),
			NowSec = util:now(),
			Mail = #mail{content=util:words_filter(Content),isRead=false,mailReward=[],mailTemplateID=0,mailType=?MAIL_TYPE_PRIVATE,mailUID=MailUID,paramList=[],senderID=RoleID,senderName=SenderName,time=NowSec},
			add_addList(TargetRoleID2, Mail),
			?unicast(RoleID, #sc_mail_new{result=1})
	end;

do_handle_info({client_msg, RoleID, #cs_mail_agree_friend{mailUID=MailUID}})->
	case lists:member(MailUID, get_delList()) of
		false ->
			case get_mail_dtl(RoleID, MailUID) of
				false ->
					?unicast(RoleID, #sc_mail_agree_friend{mailUID=MailUID,result=4});
				{_,_,MailType,SenderID}->
					if  MailType =/= ?MAIL_TYPE_ADD_FRIEND ->
							?unicast(RoleID, #sc_mail_agree_friend{result=4, mailUID=MailUID});
						true->
							add_delList([MailUID]),
							update_cache(RoleID, 1, MailUID),
							friend_server:add_friend(RoleID, SenderID, MailUID)
					end
			end;
		_ ->
			?unicast(RoleID, #sc_mail_agree_friend{result=4,mailUID=MailUID})
	end;

do_handle_info({client_msg, RoleID, #cs_mail_unread_num{}})->
	UN = get_unread_num(RoleID),
	?unicast(RoleID, #sc_mail_unread_num{newMailNum=UN});

do_handle_info({client_msg, RoleID, #cs_mail_invite_operate{mail_uid = MailUID,operate_type=OperateType}})->
    case lists:member(MailUID, get_delList()) of
        false ->
            case get_mail_dtl_more(RoleID, MailUID) of
                false ->
                    ?INFO("找不到邮件(~w)",[RoleID]),
                    ?unicast(RoleID, #sc_mail_invite_operate{result=3});
                {_,_,MailType,SenderID,[_SenderName,_FamilyName,FamilyID]}->
                    if  MailType =/= ?MAIL_TYPE_INVITE ->
                            ?INFO("邮件类型不对(~w)",[RoleID]),
                            ?unicast(RoleID, #sc_mail_invite_operate{result=3});
                        true->
                            add_delList([MailUID]),
                            update_cache(RoleID, 1, MailUID),
                            case ets:lookup(?ETS_FAMILY_INVITE,{RoleID,FamilyID}) of
                                [] ->
                                    ?INFO("邀请信息已经在ets中找不到了(~w)",[RoleID]),
                                    ?unicast(RoleID, #sc_mail_invite_operate{result=3});
                                [_] ->
                                    ets:delete(?ETS_FAMILY_INVITE,{RoleID,FamilyID}),
                                    case OperateType of
                                        1 ->
%%                                             role_lib:send_server(RoleID,{do_family_invite, FamilyID});
                                            %直接加入，越过审核环节
                                            family_misc:router_to_family_process(FamilyID, {agree_invite_join, SenderID , RoleID, FamilyID});
                                        2 ->
                                            ?INFO("成功拒绝邀请(~w)",[RoleID]),
                                            ?unicast(RoleID, #sc_mail_invite_operate{result=2})
                                    end
                            end                            
                    end
            end;
        _ ->
            ?INFO("邮件已经被删除(~w)",[RoleID]),
            ?unicast(RoleID, #sc_mail_invite_operate{result=3})
    end;

do_handle_info({client_msg, RoleID, #cs_mail_draw_reward_all{}})->
    {Reason,RewardList} = all_mail_draw_reward(RoleID),
    ?unicast(RoleID, #sc_mail_draw_reward_all{reward=RewardList,result=Reason});

do_handle_info({send_mail, SenderID, SenderName, RoleID, MailType, TemplateID, ArgList, Content, Reward})->
    MailUID = tk_id:gen_mailUID(),
    NowSec = util:now(),
    Mail = #mail{content=util:latin1(Content), isRead=false, mailReward=Reward, mailTemplateID=TemplateID, mailType=MailType, mailUID=MailUID, paramList=ArgList, senderID=SenderID, senderName=SenderName, time=NowSec},
    ?INFO("~w,\n~w,\n~w,\n~w,\n~w,\n~w,\n~w,\n~w,\n~w\n",[Content,Reward,TemplateID,MailType,MailUID,ArgList,SenderID,SenderName,NowSec]),
    add_addList(RoleID, Mail);

do_handle_info({send_role_id_list_mail, SenderID, SenderName, RoleIDList, MailType, TemplateID, ArgList, Content, Reward})->
    NowSec = util:now(),
    MailList =
        lists:foldr(
          fun(RoleID, Acc) ->
                  case RoleID >= tk_id:robot_roleID_max() of
                      true ->
                          MailUID = tk_id:gen_mailUID(),
                          Mail = #mail{content=util:latin1(Content), isRead=false, mailReward=Reward, mailTemplateID=TemplateID, mailType=MailType, mailUID=MailUID,
                                       paramList=ArgList, senderID=SenderID, senderName=SenderName, time=NowSec},
                          [{RoleID, Mail}|Acc];
                      false ->
                          Acc
                  end
          end, [], RoleIDList),
    set_addList(lists:append(MailList, get_addList()));

%% 修改内存中的缓存数据,再修改数据库中对应数据
do_handle_info({update_role_name, RoleID, Name}) ->
    lists:foreach(fun(Type) ->
                    case get({RoleID,Type}) of
                        List when is_list(List) -> 
                           case lists:keytake(RoleID, #mail.senderID, List) of
                                false ->
                                    ignore;
                                {value, Info, OtherList} ->
                                    NewList = [Info#mail{senderName=Name}|OtherList],
                                    set_mail({RoleID,Type}, NewList)
                            end;
                        _ ->
                            ignore
                    end
                  end,?MAIL_TYPE_LIST),
    db_sql:update_role_name_in_mail(RoleID,Name).  

get_mail_dtl(RoleID, MailUID)->
	Mail = db_sql:sql_find_mail(MailUID),
	[RecvID,MailTemplateID,Reward,MailType,SenderID] = Mail,
	if 	RecvID =:= RoleID ->
			{Reward,MailTemplateID,MailType,SenderID};
		true->
			false
	end.

get_mail_dtl_more(RoleID, MailUID)->
    Mail = db_sql:sql_find_mail_dtl_more(MailUID),
    [RecvID,MailTemplateID,Reward,MailType,SenderID,AgrsList] = Mail,
    if  RecvID =:= RoleID ->
            {Reward,MailTemplateID,MailType,SenderID,AgrsList};
        true->
            false
    end.

sync_mail(MailList, ClientTopMailUID)->
	sync_mail(MailList, [], 0, 0, ClientTopMailUID, []).
sync_mail([], Send, _SendNum, _DecNum, _ClientTopMailUID, NewReadMailUidList)->
	SendMail = lists:reverse(Send),
	{SendMail, [], false, NewReadMailUidList};
sync_mail(List, Send, ?MAIL_NUM_SYNC_NUM, _DecNum, ClientTopMailUID, NewReadMailUidList)->
	SendMail = lists:reverse(Send),
	DoDiscardData = 
		case List of
			[#mail{mailUID = ClientTopMailUID}|_]->
				true;
			_ ->
				false
		end,
	{SendMail, List, DoDiscardData, NewReadMailUidList};
sync_mail([M|List]=L2, Send, SendNum, DecNum, ClientTopMailUID, NewReadMailUidList)->
	#mail{mailUID=MailUID}=M,
	if 	MailUID =:= ClientTopMailUID ->
			SendMail = lists:reverse(Send),
			{SendMail, L2, false, NewReadMailUidList};
		true->
			if M#mail.isRead =:= true ->
					sync_mail(List, [M|Send], SendNum+1, DecNum, ClientTopMailUID, NewReadMailUidList);
				true->
					sync_mail(List, [M#mail{isRead=true}|Send], SendNum+1, DecNum+1, ClientTopMailUID, [MailUID|NewReadMailUidList])
			end
	end.

update_mail(RoleID, Type, CerMailList2)->
	set_mail({RoleID, Type}, CerMailList2),
	UN = get_unread_num(RoleID),
	#d_mail{unreadNum=UN}.

clear_buff() ->
	lists:foreach(fun({{RoleID,A},T}) when is_integer(RoleID) andalso is_list(T)->
							erlang:erase({RoleID, A});
						(_)->
							ignore
						end, erlang:get()).

tick() ->
	erlang:send_after(?TICK_INTERVAL*1000, self(), tick).

dump_tick() ->
	erlang:send_after(?DUMP_INTERVAL*1000, self(), dump_tick).

transform_p_mail_list(MailList)->
	[transform_p_mail(E)||E<-MailList].
transform_p_mail(Mail)->
	#mail{content=C,mailReward=R,mailTemplateID=T,mailType=Ty,mailUID=ID,paramList=Pr,senderID=SID,senderName=SN,time=Ti,head=Head,isMale=IsMale0} = Mail,
    IsMale = case IsMale0 of
                 0 ->
                     false;
                 1 ->
                     true;
                 false ->
                     false;
                 true ->
                     true
             end,
	#p_mail{content=util:words_exchange(C),mailReward=[role_reward:transform2p_mail_reward(R)],mailTemplateID=T,mailType=Ty,mailUID=ID,paramList=Pr,senderID=SID,senderName=SN,time=Ti,head=Head,isMale=IsMale}.

sql_sync_read([]) -> ignore;
sql_sync_read(List)->
	db_sql:read_mail(List).

init_client_mail(MailList)->
	init_client_mail(MailList, [], 0, 0, []).
init_client_mail([], Send, _SendNum, _DecNum, NewReadMailUidList)->
	SendMail = lists:reverse(Send),
	{SendMail, [], NewReadMailUidList};
init_client_mail(List, Send, ?MAIL_NUM_NODE_NUM, _DecNum, NewReadMailUidList)->
	SendMail = lists:reverse(Send),
	{SendMail, List, NewReadMailUidList};
init_client_mail([M1|List], Send, SendNum, DecNum, NewReadMailUidList)->
	M = case is_record(M1,mail) of
		true->
			M1;
		false->
			Temp = tuple_to_list(M1),
			list_to_tuple(Temp++[0,false])
	end,
	#mail{isRead = IsRead, mailUID = MailUID} = M,
	if IsRead =:= true ->
		init_client_mail(List, [M|Send], SendNum + 1, DecNum, NewReadMailUidList);
	true->
		init_client_mail(List, [M#mail{isRead = true}|Send], SendNum + 1, DecNum + 1, [MailUID|NewReadMailUidList])
	end.

get_last_n_mail(RoleID,Type, ClientTopMailUID) ->
    DelList = get_delList(),
	#d_mail{mail=[ML]} = Mail = db_sql:get_last_mails_by_type(RoleID,Type,ClientTopMailUID),
    ML2 = lists:filter(fun(M)-> lists:member(M#mail.mailUID, DelList) =:= false end, ML),
    Mail#d_mail{mail=[ML2]}.

set_mail(Key, Val)->
	erlang:put(Key, Val).

%% add_mail(RoleID, _Type, Mail)->
%% 	db_sql:add_mailList([{RoleID, Mail}]).

add_delList([])->ignore;
add_delList(List)->
	set_delList(List++get_delList()).

set_delList(List)->
	erlang:put(?DEL_LIST, List).

get_delList()->
	case erlang:get(?DEL_LIST) of
		[_|_] = List->
			List;
		_ ->
			[]
	end.

update_cache(RoleID, Type, MailUID)->
	Mails=get_mail(RoleID, Type),
	case lists:keytake(MailUID, #mail.mailUID, Mails) of
		{value, _Mail, Mails2} ->
			set_mail({RoleID, Type},Mails2);
		false ->
			ok
	end.

update_cache2(RoleID, Type, MailUIDList) ->
    Mails = get_mail(RoleID, Type),
    NewMails =
        lists:foldr(
          fun(MailUID, AccMails) ->
                  case lists:keytake(MailUID, #mail.mailUID, AccMails) of
                      {value, _Mail, NewAccMails} ->
                          NewAccMails;
                      false ->
                          AccMails
                  end
          end, Mails, MailUIDList),
    set_mail({RoleID, Type}, NewMails).

%% when client ask for new mail, we find it from sql, so when we add a mail we don't need to update cache
%% but when we delete a mail, it is possible in the cache... when client ask for more mails, we use cache 
add_addList(RoleID, Mail)->
	set_addList([{RoleID, Mail}|get_addList()]).

get_addList()->
	case erlang:get(?ADD_LIST) of
		[_|_] = List ->
			List;
		_ ->
			[]
	end.
		
set_addList(List) ->
	erlang:put(?ADD_LIST, List).

get_mail(RoleID, Type)->
	case erlang:get({RoleID, Type}) of
		?undefined ->
			[];
		T_List ->
			T_List
	end.

get_unread_num(RoleID)->
	UnSys = db_sql:get_unread_num(RoleID,1),
	UnPriv = db_sql:get_unread_num(RoleID,2),
    UnFriendReq = db_sql:get_unread_num(RoleID,3),
	[UnSys, UnPriv, UnFriendReq].

get_more_mail(RoleID, Type, StartMailUID)->
    DelList = get_delList(),
	Mails=get_mail(RoleID, Type),
	{Mails2, Num} = lists:foldl(fun(E,{L,Acc})-> 
									{[E|L],Acc+1}
								end, {[],0},Mails),
	Mails4 = if  Num < ?MAIL_NUM_NODE_NUM ->
			if 	Num > 0 ->
					[Mail|_] = Mails2,
					StartMailUID2 = Mail#mail.mailUID;
				true ->
					StartMailUID2 = StartMailUID
			end,
			Mails3 = db_sql:sql_get_more_mail(RoleID, Type, StartMailUID2),
			Mails ++ Mails3;
		true ->
			Mails
	end,
    lists:filter(fun(M)-> lists:member(M#mail.mailUID, DelList) =:= false end, Mails4).

get_targetRoleID(0, TargetRoleName) ->
	case db_sql:search_roleName(TargetRoleName) of
		?undefined ->
			0;
		RoleID ->
			RoleID
	end;
get_targetRoleID(TargetRoleID,_TargetRoleName) ->
	TargetRoleID.

do_write_db() ->
	AddList = get_addList(),
	db_sql:add_mailList(AddList),
	set_addList([]),
	spawn(fun()->
				  lists:foreach(fun({RoleID, _})->
										case role_lib:is_online(RoleID) of
											true ->
												timer:sleep(10),
												UN=get_unread_num(RoleID),
												?unicast(RoleID, #sc_mail_unread_num{newMailNum=UN});
											_ ->
												ignore
										end
								end, AddList)
		  end),
	db_sql:del_mail(get_delList()),
	set_delList([]),
    case get(?send_strange_count) of
        SendStrangeCount when erlang:is_record(SendStrangeCount, send_strange_count) ->
            db_sql:set_etc(?DB_ETC_KEY_MAIL_SERVER,[SendStrangeCount]);
        _ ->
            ignore
    end,
	ok.

check_mail_draw_reward(RoleID,Reward, MailTemplateID)->
    case MailTemplateID of
        ?MAIL_PLUNDER_STONECHIP_RECOVER ->
			#sell_reward{item=ItemList}=Reward,
            [{new_item,ProductTypeID,AddNum,_,_}|_] = ItemList,
            CurNum = plunder_server:get_stonechip_num(RoleID, ProductTypeID),
            MinNum = data_stonechip_fight:get(each_max_num),
            if
                CurNum + AddNum > MinNum ->
                    case data_patch_parts:get(ProductTypeID) of
                        undefined ->
                            {ok,#sell_reward{}};
                        {StoneProductTypeID,Position}->
                            case data_patch_product:get({StoneProductTypeID,(Position+util:random_int(1, 3)) rem 4}) of
                                undefined ->
                                    {ok,#sell_reward{}};
                                NewProductTypeID->
                                    NewReward = {sell_reward,0,0,0,0,[{new_item,NewProductTypeID,AddNum,1,0}],0,[]},
                                    {ok,NewReward}
                            end
                    end;
                true ->
                    {ok,Reward}
            end;
        _ ->
            {ok,Reward}
    end.

all_mail_draw_reward(RoleID)->
    MailDetailList1 = get_mail(RoleID, 1),
    case MailDetailList1 of
        [] ->
            #d_mail{mail=[MailDetailList2]} = get_last_n_mail(RoleID, 1, 0);
        _ ->    
            #d_mail{mail=[MailDetailList2]} = get_last_n_mail(RoleID, 1, lists:max([M#mail.mailUID||M<-MailDetailList1]))
    end,
    sql_sync_read([M#mail.mailUID||M<-MailDetailList2]),
%%     case MailDetailList1++MailDetailList2 of
    MailDetailList = lists:filter(fun(MDetail)-> false =:= lists:member(MDetail#mail.mailUID,MailDetailList1) end, MailDetailList2),
    case MailDetailList of
        [] ->
            ?INFO("all_mail_draw_reward blank",[]),
            {2,[]};
        _ ->
            %% 防止没处理完玩家就下线了
            case catch(all_mail_draw_reward2(RoleID,MailDetailList)) of
                RewardList when erlang:is_list(RewardList)->
                    ?INFO("all_mail_draw_reward ~w",[RewardList]),
                    {1,RewardList};
                ERR ->
                    ?INFO("all_mail_draw_reward error ~w",[ERR]),
                    {2,[]}
            end 
    end.

all_mail_draw_reward2(RoleID,MailDetailList)->
    lists:foldl(fun(MailDetail,AccRewardList)-> 
                    M = case is_record(MailDetail,mail) of
                        true->
                            MailDetail;
                        false->
                            Temp = tuple_to_list(MailDetail),
                            list_to_tuple(Temp++[0,false])
                    end,
                    #mail{mailUID = MailUID, mailTemplateID = MailTemplateID, mailReward = MailReward} = M,
                    case lists:member(MailUID, get_delList()) of
                        false ->
                            {ok,Reward} = check_mail_draw_reward(RoleID,MailReward, MailTemplateID),
                            add_delList([MailUID]),
                            update_cache(RoleID, 1, MailUID),                        
                            if
                                MailTemplateID =:= ?MAIL_FAMILY_BE_AUTO_DISBAND ->
                                    del_all_family_disband_mail(RoleID);
                                true ->
                                    ignore
                            end,
							?INFO("all_mail_draw_reward2 foreach ~w",[Reward]),
                            if
                                Reward =/= [] ->
                                    erlang:send(role_lib:regName(RoleID), {draw_mail_reward,Reward,MailTemplateID}),
                                    [activity_server:sell_reward2p_reward_info(Reward)|AccRewardList];
                                true ->
                                    AccRewardList
                            end;
                        true ->
                            AccRewardList
                    end
                  end, [], MailDetailList).

check_strange_count(RoleID)->
    StrangeCount = get(?send_strange_count),
    Today = erlang:date(),
    Ret = if
        StrangeCount#send_strange_count.date =:= Today ->
            case lists:keytake(RoleID,1,StrangeCount#send_strange_count.count_list) of
                false ->
                    StrangeCount#send_strange_count{count_list = [{RoleID,1}|StrangeCount#send_strange_count.count_list]};
                {value,{RoleID,CountReady},OtherList} ->
                    SendStrangeCountMax = data_talk:get(send_strange_count_max),
                    if
                        CountReady >= SendStrangeCountMax ->
                            false;
                        true ->
                            StrangeCount#send_strange_count{count_list = [{RoleID,CountReady+1}|OtherList]}
                    end
            end;
        true ->
            #send_strange_count{date = Today
                               ,count_list = [{RoleID,1}]}
    end.
                    
%% 修补公会解散邮件发送多封的问题，下面函数仅在领取了解散返还钻石后，被调用，删除其他公会解散的补偿邮件
%% 由于暂时不能更改db_sql.erl所以无法彻底删除数据库中邮件。只是防止玩家领取邮件
del_all_family_disband_mail(RoleID)->
    #d_mail{mail=[MailList]} = get_last_n_mail(RoleID, 1,0), 
    lists:foreach(fun(#mail{mailTemplateID=MailTemplateID,mailUID=MailUID})->
                    if
                        MailTemplateID =:= ?MAIL_FAMILY_BE_AUTO_DISBAND ->
                            add_delList([MailUID]),
                            update_cache(RoleID, 1, MailUID);
                        true->
                            next
                    end
    end, MailList).
