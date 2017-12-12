%% @author lijunfeng

-module(pay_mod_yyg2).
-include("common.hrl").

%%旧的SDK参数
-define( APPKEY, "dcfa3cf30ebfa40a43b94bfbd1f3f046" ).

%%新的SDK参数
-define(CP_KEY,"3VMxSNGxOfrYc5yYNdCV").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case check_verify( QueryString ) of
		{true,RoleID,Amount} ->
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				59 ->
					Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply}),
					QS = mochiweb_util:urlencode(QueryString),
					Sign2 = util:md5(proplists:get_value("sign", QueryString)),
					pay_gold2(RoleID,Amount,QS,Sign2,59);	
				_->
					QS = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		{false,Error} ->
            Reply = ejson:encode({[{<<"result">>,Error}]}),
            Req:ok({"text/html; charset=utf-8", Reply}),
             ?ERR("yyg2 pay failed. reason:sign wrong,order:~w",[QueryString])
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_yyg(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
%% 	?ERR("pay_gold2 yyg err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].

check_verify(QueryString)->
    Sign = proplists:get_value("sign",QueryString),
    Data = proplists:get_value("data",QueryString),
    Sign2 = string:to_lower(util:md5(Data++";"++?CP_KEY)),
    case Sign2=:=Sign of
        false->
            {false,-11};
        true->
            analyse_data(Data)
    end.

analyse_data(Data)->
    {Content} = ejson:decode(Data),
    Remark = binary_to_list(get_value(Content,<<"remark">>)),
    Amount = get_value(Content,<<"amount">>),
    [RoleIDStr,_ServerIDStr|_T] = string:tokens(Remark,"_"),
    {true,list_to_integer(RoleIDStr),trunc(Amount*10)}.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

