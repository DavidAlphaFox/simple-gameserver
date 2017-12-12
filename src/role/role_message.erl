%% @author caohongyang
%% @doc 公告、信息
%% Created 2013-4-1


-module(role_message).
-compile(export_all).
-include("def_role.hrl").
-include("def_battle.hrl").
%% API functions
-export([]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

cs_message_notice(#cs_message_notice{curMaxNoticeID=CurMaxNoticeID}) ->
	IDList = data_notice:get_list(),
	case lists:member(CurMaxNoticeID,IDList) of
	  	 false ->
			Content = [data_notice:get(E)||E<-IDList];
		 _ ->
			Content = []
	end,		 
	Reply = #sc_message_notice{noticeIDList=IDList,noticeList=[], notice=ejson:encode((hd(Content))#p_notice.content)},
	?sendself(Reply).

cs_message_certain_notice(#cs_message_certain_notice{noticeIDList=NoticeIDList}) ->
	Reply = #sc_message_certain_notice{noticeList=[data_notice:get(E)||E<-NoticeIDList]},
	?sendself(Reply).
	
							  
cs_message_test(#cs_message_test{msg=Msg}) ->
	%% 正式发布版，gm命令无效
	case data_setting:get(is_release) of
		false ->
			role_gm:test(Msg);
		_ ->
			ignore
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

data_notice_transform_list(List) ->
	[data_notice_transform(E)||E<-List].

data_notice_transform(Notice) ->
	#notice{content=C,noticeID=I,title=T} = Notice,
	#p_notice{content=C,noticeID=I,title=T}.
