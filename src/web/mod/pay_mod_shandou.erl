%% @author zcl
%% @doc shandou支付处理
%% Created 2014/6/11


-module(pay_mod_shandou).
-include("common.hrl").

%%游戏定义的渠道
-define(ACCOUNT_TYPE_SHANDOU,94).               %闪豆
%%游戏定义的渠道结束


-export([pay_gold/1]).


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
        {true,RoleID,Amount,Receipt,Sign} ->
            {RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				?ACCOUNT_TYPE_SHANDOU ->
					pay_gold2(RoleID, Amount, Receipt, Sign, ?ACCOUNT_TYPE_SHANDOU),
					Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply});
				_ ->
					QS = mochiweb_util:urlencode(QueryString),
					?ERR("wrong src Type:~w,~w,~s",[RoleID,RRType,QS]),
					Reply = ejson:encode({[{<<"result">>,5},{<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply})
			end;
		_ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

pay_gold2(RoleID, Amount, Receipt, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_shandou(RoleID,Amount,Receipt, Sign,SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_sign(QueryString)->
    SecretKey = "5f93f983524def3dca464469d2cf9f3e",
    AppID = proplists:get_value("appid", QueryString),
    InnerOrderNo = proplists:get_value("inner_orderno", QueryString),
    OrderInfo = proplists:get_value("cp_orderno", QueryString),
    Content = proplists:get_value("content", QueryString),
    Sign = proplists:get_value("sign",QueryString),
    case not lists:member(?undefined, [AppID,InnerOrderNo,OrderInfo,Content,Sign]) of
        false ->
            ?ERR("exit undefine value：APPID：~w InnerOrderNo:~w OrderInfo:~w Content:~wSign:~w ~n",[AppID,InnerOrderNo,OrderInfo,Content,Sign]),
            false;
        true ->
            CaculateSign = caculateSign(AppID,OrderInfo,InnerOrderNo,SecretKey),
            case CaculateSign =:=Sign of 
                true ->
                    PayAmount = get_pay_amount(Content) div 10,
                    [_,_ServerID,RoleID] = string:tokens(OrderInfo,"A"),
                    {true,list_to_integer(RoleID),PayAmount,OrderInfo,Sign};
                false ->
                    ?ERR("sign is not same： caculateSign:~w Receive sign :~w ~n",[CaculateSign,Sign]),
                    false
            end
    end.

caculateSign(AppID,OrderInfo,InnerOrderNo,SecretKey)->
    md5(AppID++OrderInfo++InnerOrderNo++SecretKey).

get_pay_amount(Content)->
    RealContent = base64:decode(Content),
    {RealContent2} = ejson:decode(RealContent),
    binary_to_integer(get_value(RealContent2,<<"payspnum">>)).

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.
%%提取xml中充值需要的数据
% get_info(Data)-> 
%     {ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(Data),
%     [{_,_,_,_,ChannelID,_}] = xmerl_xpath:string("//channel/text()",ParsedDocumentRootElement),
%     [{_,_,_,_,Amount,_}] = xmerl_xpath:string("//amount/text()",ParsedDocumentRootElement),
%     [{_,_,_,_,Status,_}] = xmerl_xpath:string("//status/text()",ParsedDocumentRootElement),
%     [{_,_,_,_,ExtraInfo,_}] = xmerl_xpath:string("//extras_params/text()",ParsedDocumentRootElement),
%     [{_,_,_,_,OrderNo,_}] = xmerl_xpath:string("//order_no/text()",ParsedDocumentRootElement),
%     case Status of
%         "0"->
%             case quick_channel_to_our_channele(ChannelID) of
%                 false ->
%                     false;
%                 OurChannel->
%                     [_,_,RoleID] = string:tokens(ExtraInfo,"A"),
%                     {true,list_to_integer(RoleID),trunc(list_to_float(Amount)),OurChannel,OrderNo}
%             end;
%         "1"->
%             false
%     end.
