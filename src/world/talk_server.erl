%% @author : lixinglong
%% @doc : 聊天服务器，处理gm禁言信息

-module(talk_server).
-behaviour(gen_server).
-include("common.hrl").
-include("all_proto.hrl").
-include("data.hrl").
-include("record.hrl").

-compile(export_all).

-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([i/0, is_gaged/1, gag_one/1]).

-define(DUMP_INTERVAL, 300).%写数据库的间隔,单位：秒
-define(TALK_LOG_INTERVAL, 5). %聊天记录写文件间隔
-define(RECENT_NUM, 50).     %%缓存的聊天记录数

-record(state, {gag_list=[], recent_talk_data=[], talk_log=[]}).

is_gaged(RoleID)->
	gen_server:call(?MODULE, {is_gaged, RoleID}).

gag_one(RoleID)->
	erlang:send(?MODULE, {gag_one, RoleID}).

ungag_one(RoleID)->
	erlang:send(?MODULE, {ungag_one, RoleID}).

get_gag_list()->
	gen_server:call(?MODULE, get_gag_list).

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
	State=get_db_info(),
	process_flag(trap_exit,true),
	dump_tick(),
	erlang:send_after(?TALK_LOG_INTERVAL * 1000, self(), write_talk_log),
	State2 = update_state(State),
	{ok, State2}.

update_state(State)->
	#state{recent_talk_data=List} = State,
	List2 = lists:foldl(fun({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location},Acc)->
								{Head1,IsMale1,Level1} = 
									case role_lib:get_rolePublic(RoleID) of
										#rolePublic{head=Head,isMale=IsMale,level = Level} ->
											{Head,IsMale,Level};
										_ ->
											{0,false,1}
									end,
                                Vip = role_lib:get_vip_level(RoleID),
                                Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
								[{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip,Grade,Level1}|Acc];
                           ({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1},Acc)->
                                Vip = role_lib:get_vip_level(RoleID),
                                #rolePublic{level = Level} = role_lib:get_rolePublic(RoleID),
                                Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
                                [{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip,Grade,Level}|Acc];
						   ({sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip},Acc)->
                                #rolePublic{level = Level} = role_lib:get_rolePublic(RoleID),
                                Grade = doublematch_server:dirty_get_role_dm_grade(RoleID),
								[{sc_talk_world_message,Channel,RoleName,Message,RoleTitle,TimeStamp,RoleID,FamilyTitle,Location,Head1,IsMale1,Vip,Grade,Level}|Acc];
						   (Msg,Acc) ->
								[Msg|Acc]
						end, [], List),
	State#state{recent_talk_data=List2}.

handle_call(i, _From, State) ->
	{reply, State, State};
handle_call({is_gaged, RoleID},_From, State)->
	Result = lists:member(RoleID, State#state.gag_list),
	{reply, Result, State};
handle_call(get_gag_list,_From, State)->
	{reply, State#state.gag_list, State};
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
	{noreply, State}.

handle_info({inet_reply,_S,_Status},State) ->
    {noreply,State};

handle_info({Ref,_Res},State) when is_reference(Ref) ->
    {noreply,State};
        
handle_info({talk_channel_world, Data}, #state{recent_talk_data=RecentTalkData, talk_log=TalkLog}=State)->
	NewRecentTalkData = lists:sublist([Data|RecentTalkData], ?RECENT_NUM),
	NewTalkLog = [Data|TalkLog],
	{noreply, State#state{recent_talk_data=NewRecentTalkData, talk_log=NewTalkLog}};
handle_info({talk_channel_family, Data}, #state{talk_log=TalkLog}=State) ->
    NewTalkLog = [Data|TalkLog],
    {noreply, State#state{talk_log=NewTalkLog}};
handle_info({client_msg, RoleID, #cs_talk_recent_list{channel=Channel}}, #state{recent_talk_data=List}=State)->
    case Channel of
        ?CHAT_CHANNEL_WORLD ->
            ?unicast(RoleID, #sc_talk_recent_list{list=lists:reverse(List),channel=?CHAT_CHANNEL_WORLD});
        ?CHAT_CHANNEL_FAMILY ->
            case erlang:whereis(role_lib:regName(RoleID)) of
                ?undefined ->
                    ?unicast(RoleID, #sc_talk_recent_list{list=[],channel=?CHAT_CHANNEL_FAMILY});
                _ ->
                    role_lib:send_server(RoleID, get_family_recent_talk)
            end;
        _ ->
            next
    end,
    {noreply, State};
handle_info(write_talk_log, #state{talk_log=TalkLog}=State)->
	erlang:send_after(?TALK_LOG_INTERVAL * 1000, self(), write_talk_log),
	do_write_talk_data(TalkLog),
	{noreply, State#state{talk_log=[]}};
handle_info(dump_tick, State)->
	do_write_db(State),
	dump_tick(),
	{noreply, State, hibernate};

handle_info({gag_one, RoleID}, #state{gag_list=GagList}=State)->
	GagList2 =
	case lists:member(RoleID, GagList) of
		true ->
			GagList;
		false ->
			[RoleID|GagList]
	end,
	{noreply, State#state{gag_list=GagList2}};
handle_info({ungag_one, RoleID}, #state{gag_list=GagList}=State)->
	GagList2 = lists:delete(RoleID, GagList),
	{noreply, State#state{gag_list=GagList2}};
handle_info({update_role_name, RoleID, Name}, #state{recent_talk_data=RecentTalkData}=State) ->
    NewData = lists:foldr(fun({sc_talk_world_message,_Channel,_RoleName,_Message,_RoleTitle,_TimeStamp,RoleIDT,_FamilyTitle,_Location,_Head,_IsMale,_VipL,_,_} = Info, Acc) ->
                                case RoleID =:= RoleIDT of
                                    true ->
                                        [Info#sc_talk_world_message{roleName=Name}|Acc];
                                    false ->
                                        [Info|Acc]
                                end
                          end, [], RecentTalkData),
    NewState = State#state{recent_talk_data=NewData},
    {noreply, NewState};
handle_info(_Info, State) ->
	%?CATCH(do_handle_info(Info)),
	{noreply, State}.

terminate(Reason, State) ->
	?INFO("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[?MODULE, Reason,  State, element(2,process_info(self(),dictionary))]), 
	do_write_db(State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

dump_tick() ->
	erlang:send_after(?DUMP_INTERVAL*1000, self(), dump_tick).

get_db_info()->
	case db_sql:get_etc(?DB_ETC_KEY_TALK) of
		X when erlang:is_record(X, state) ->
			X;
		_ ->
			#state{}
	end.

do_write_db(State) ->
	db_sql:set_etc(?DB_ETC_KEY_TALK, State).

do_write_talk_data([]) ->
	ok;
do_write_talk_data(DataList) ->
	NewDataList =
		[util:to_list(RoleName) ++ " " ++ Message ++ " " ++ erlang:integer_to_list(Title) ++ " " ++ erlang:integer_to_list(Timestamp)
			 ++ " " ++ erlang:integer_to_list(RoleID) ++ " " ++ util:to_list(Location) ++"\n"
		 ||#sc_talk_world_message{roleName=RoleName,message=Message,roleTitle=Title,timeStamp=Timestamp,roleID=RoleID,location=Location}<-lists:reverse(DataList)],
	FileName = make_log_file(),
	file:write_file(FileName, NewDataList, [append]).

%%生成日志文件名
make_log_file() ->
    {{Year, Month, Day}, {Hour, _, _}} = erlang:localtime(),
	LogDir = get_log_dir(),
    filename:join([LogDir, io_lib:format("talk_~p_~p_~p_~p", [Year, Month, Day, Hour])]).

get_log_dir() ->
	case file:consult(filename:join([tk_config:root_dir(),"setting","setting.config"])) of
		{ok, KVList} ->
			case lists:keyfind(logger_file_dir, 1, KVList) of
				false ->
					"/data/log";
				{_,Dir} ->
					Dir
			end;
		_ ->
			"/data/log"
	end.


test_info()->
	A = [{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,";",0,1407920459,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,"?",0,1407920466,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,"!",0,1407920472,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,"\"'",0,1407920480,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,",",0,1407920485,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,".",0,1407920491,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,[44,46,92,226,128,166,58,59,33,34],0,1407920533,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,129,165,229,186,183,231,154,132,229,184,131,233,155,183,230,129,169>>,[64,94,126,41,35,37,226,130,169,194,163,194,165,47],0,1407920546,5012293,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<228,185,150,228,185,150,229,165,179,228,185,148,229,174,137,229,168,156>>,[232,191,135,230,149,143,231,186,162,231,177,179,119,112,115,229,147,166,229,146,175],0,1408071534,5012371,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<232,131,150,228,185,142,228,185,142,231,154,132,228,184,152,229,144,137,229,176,148>>,"jdjdjdsdndmka..",0,1408340710,5012387,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130]}
        ,{sc_talk_world_message,1,<<229,189,170,230,130,141,231,154,132,233,155,183,229,133,139>>,"***",0,1409405002,5012448,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<229,189,170,230,130,141,231,154,132,233,155,183,229,133,139>>,"1",0,1409405434,5012448,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<229,189,170,230,130,141,231,154,132,233,155,183,229,133,139>>,[229,149,138,228,189,160,230,152,175,228,184,141,230,152,175,229,149,138],0,1409405625,5012448,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<229,143,175,231,136,177,231,154,132,233,152,191,232,138,153,230,139,137>>,[229,129,165,229,186,183,229,191,171,228,185,144,229,129,165,229,186,183,229,191,171,228,185,144,229,144,144,233,156,178],0,1409452197,5012442,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<229,143,175,231,136,177,231,154,132,233,152,191,232,138,153,230,139,137>>,[229,147,166,229,155,190,229,155,190,230,136,145,233,187,152,233,187,152],0,1409452206,5012442,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<231,136,177,232,175,180,232,176,142,231,154,132,233,169,172,230,150,135,231,154,132,233,162,157>>,[231,156,139,231,156,139,229,142,187],0,1409452496,5012472,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<231,136,177,232,175,180,232,176,142,231,154,132,233,169,172,230,150,135,231,154,132,233,162,157>>,[229,136,169,230,182,166],0,1409452518,5012472,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<229,143,175,231,136,177,231,154,132,233,152,191,232,138,153,230,139,137>>,[229,129,165,229,186,183,229,191,171,228,185,144,229,162,168],0,1409452537,5012442,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<231,136,177,232,175,180,232,176,142,231,154,132,233,169,172,230,150,135,231,154,132,233,162,157>>,[230,178,161,230,181,129,233,135,143,228,186,134],0,1409452587,5012472,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<232,144,140,229,138,168,229,176,143,229,166,158,230,156,177,228,184,189,229,143,182,231,144,179>>,[229,165,189,229,143,139],0,1409452668,5012466,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<232,144,140,229,138,168,229,176,143,229,166,158,230,156,177,228,184,189,229,143,182,231,144,179>>,[230,177,130,229,165,189,229,143,139],0,1409452687,5012466,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<231,159,173,229,143,145,231,154,132,232,137,190,230,150,135>>,"aasd",0,1409457451,5012433,0,<<>>,0,false}
        ,{sc_talk_world_message,1,<<229,143,175,231,136,177,231,154,132,233,152,191,232,138,153,230,139,137>>,[229,129,165,229,186,183,229,191,171,228,185,144],0,1409457594,5012442,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<229,143,175,231,136,177,231,154,132,233,152,191,232,138,153,230,139,137>>,[229,149,138,231,136,184,231,136,184,229,149,166,229,149,138,229,144,167],0,1409457610,5012442,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,false}
        ,{sc_talk_world_message,1,<<229,183,168,230,152,159,228,186,171,229,136,169>>,[233,151,183,231,131,173,228,186,134,229,144,151,229,149,138,232,183,175,228,187,150],0,1409469209,5012486,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<229,183,168,230,152,159,228,186,171,229,136,169>>,[233,165,191,233,133,183,230,136,145],0,1409472685,5012486,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<229,183,168,230,152,159,228,186,171,229,136,169>>,[229,147,166,232,183,175,230,136,145,233,165,191],0,1409472782,5012486,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<231,157,191,230,153,186,231,154,132,229,159,131,229,176,148,231,187,180>>,[229,147,166,228,184,141,228,185,159,229,144,131],0,1409472884,5012488,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<231,157,191,230,153,186,231,154,132,229,159,131,229,176,148,231,187,180>>,[231,190,142,229,137,167],0,1409473093,5012488,0,[229,155,155,229,183,157,231,156,129,230,136,144,233,131,189,229,184,130],0,true}
        ,{sc_talk_world_message,1,<<231,180,171,231,189,151,229,133,176,232,142,137,229,184,140,228,186,154>>,[229,149,138,229,149,138,229,149,138],0,1409540718,5012534,0,<<>>,0,false}
        ,{sc_talk_world_message,1,<<231,180,171,231,189,151,229,133,176,232,142,137,229,184,140,228,186,154>>,[229,149,138,229,149,138,229,149,138],0,1409540725,5012534,0,<<>>,0,false}
        ,{sc_talk_world_message,1,<<231,180,171,231,189,151,229,133,176,232,142,137,229,184,140,228,186,154>>,[229,149,138,229,149,138,229,149,138],0,1409540776,5012534,0,<<>>,0,false,1}],
	State=#state{recent_talk_data=A},
	do_write_db(State).
