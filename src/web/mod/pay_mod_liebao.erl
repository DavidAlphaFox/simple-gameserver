%% @author zcl
%% @doc liebao支付处理
%% Created 2014/6/11


-module(pay_mod_liebao).
-include("common.hrl").

%%游戏定义的渠道
-define(ACCOUNT_TYPE_LIEBAO,97).               %猎宝
%%游戏定义的渠道结束


-export([pay_gold/1]).


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    case verify_sign(QueryString) of
        {true,RoleID,Amount,Receipt,Sign} ->
            {AccID,SrcType} = db_sql:get_role_accid_and_type(RoleID),
            case SrcType of
                ?ACCOUNT_TYPE_LIEBAO->
                    pay_gold2(RoleID, Amount*10, Receipt, Sign, ?ACCOUNT_TYPE_LIEBAO),
                    Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply});
                _->
                    Reply = ejson:encode({[{<<"result">>,5}, {<<"accid">>,AccID}]}),
                    Req:ok({"text/html; charset=utf-8", Reply}),
                    ?ERR("liebao pay for other SrcType:~w ~n",[SrcType])
            end; 
        _ ->
            Reply = ejson:encode({[{<<"result">>,0}]}),
            Req:ok({"text/html; charset=utf-8", Reply})
    end.

pay_gold2(RoleID, Amount, Receipt, Sign, SrcType) when is_integer(RoleID) ->
    pay_server:do_pay_from_liebao(RoleID,Amount,Receipt, Sign,SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_sign(QueryString)->
    OrderID = proplists:get_value("orderid",QueryString),
    UserName = proplists:get_value("username",QueryString),
    GameID = proplists:get_value("gameid",QueryString),
    RoleID = proplists:get_value("roleid",QueryString),
    ServerID = proplists:get_value("serverid",QueryString),
    PayType = proplists:get_value("paytype",QueryString),
    Amount = proplists:get_value("amount",QueryString),
    PayTime = proplists:get_value("paytime",QueryString),
    Attach = proplists:get_value("attach",QueryString),
    Sign = proplists:get_value("sign",QueryString),
    case check(OrderID,UserName,GameID,RoleID,ServerID,PayType,Amount,PayTime,Attach,Sign) of
        true->
            [RoleID2,_ServerID2,_TimeStamp] = string:tokens(Attach,"A"),
            [Amount2|_H] = string:tokens(Amount++".","."),
            {true,list_to_integer(RoleID2),list_to_integer(Amount2),mochiweb_util:urlencode(QueryString),Sign};
        false->
            false
    end.

check(OrderID,UserName,GameID,RoleID,ServerID,PayType,Amount,PayTime,Attach,Sign)->
    AppKey = "21378e414ab3e330c0bde7b814558c59",
    Str = "orderid="++OrderID++"&username="++UserName++"&gameid="++GameID++"&roleid="++RoleID++"&serverid="++ServerID++"&paytype="++PayType++"&amount="++Amount++"&paytime="++PayTime++"&attach="++Attach++"&appkey="++AppKey,
    Sign =:= md5(Str).

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