%% @author zcl
%% @doc quicksdk支付处理
%% Created 2014/6/11


-module(pay_mod_quick).
-include("common.hrl").

%%quickSDk 定义的channelID
-define(UNDEFIEND, "0").              % 默认
-define(C_SKYMOONS, "1").             % 天象
-define(C_5GWAN, "3").                % 5Gwan
-define(C_DOWNJOY, "4").              % 当乐
-define(C_PPTV, "5").                 % pptv
-define(C_APPCHINA, "6").             % 应用汇
-define(C_YOU_KU, "7").               % 优酷
-define(C_JIN_SHAN, "8").             % 金山
-define(C_UC, "9").                   % UC
-define(C_AN_ZHI, "10").              % 安智
-define(C_WAN_DOU_JIA, "11").         % 豌豆荚
-define(C_360, "12").                 % 360
-define(C_91, "13").                  % 91
-define(C_DUOKU, "14").               % 多酷
-define(C_XIAO_MI, "15").             % 小米
-define(C_MUZHIWAN, "16").            % 拇指玩
-define(C_VIVO, "17").                % vivo
-define(C_3G, "18").                  % 3G
-define(C_MUMAYI, "19").              % 木蚂蚁
-define(C_UUCUN, "20").               % 悠悠村
-define(C_EGAME, "21").               % 爱游戏
-define(C_EWAN, "22").                % 益玩
-define(C_OPPO, "23").                % oppo
-define(C_HUAWEI, "24").              % 华为
-define(C_LEYGAME, "25").             % 乐游天下
-define(C_JINLI, "26").               % 金立
-define(C_4399, "27").                % 4399
-define(C_SOGOU, "28").               % 搜狗
-define(C_LENOVO, "29").              % 联想
-define(C_LEDOU, "30").               % 乐逗
-define(C_MM, "31").                  % 移动MM
-define(C_TENCENT, "32").             % 应用宝
-define(C_PPS, "33").                 % PPS
-define(C_KUPAI, "43").               % 酷派
-define(C_HMPAY, "44").               % 海马
-define(C_SINA, "45").                % 新浪
-define(C_YUN_YOU, "46").             % 云游飞天
-define(C_SHUN_WANG, "47").           % 顺网游戏
%-define(C_SHUN_WANG_IOS, "48").		%顺网ios
-define(C_65,"49").					%65.com
-define(C_LEMENGHUDONG,"54").           %乐盟互动
-define(C_49YOU,"119").               %%49游
%%quick SDK 渠道定义结束


%%游戏定义的渠道
-define(ACCOUNT_TYPE_SINA, 20).                     % 新浪
-define(ACCOUNT_TYPE_SHUNWANG,80).                  % 顺网
-define(ACCOUNT_TYPE_49YOU,81).                     %49游
-define(ACCOUNT_TYPE_QUICKSDK,82).              %quickSDK
-define(ACCOUNT_TYPE_SHUNWANG_IOS,83).                  % 顺网IOS
-define(ACCOUNT_TYPE_YUNYOU,84).                  % 云游飞天
-define(ACCOUNT_TYPE_HM_ANDROID,85).                  % 海马Android
-define(ACCOUNT_TYPE_65, 86).						% 65.COM
-define(ACCOUNT_TYPE_QUICK_YYB,88).             %使用quick方式接入的应用宝
-define(ACCOUNT_TYPE_QUICK_LEMENGHUDONG,89).    %乐盟互动
%%游戏定义的渠道结束


-export([pay_gold/1]).


%% ====================================================================
%% API functions
%% ====================================================================

pay_gold(Req) ->
	QueryString = Req:parse_qs(),
	case verify_sign(QueryString) of
		{true,RoleID,Amount,OurChannel,Receipt,Sign} ->
			Amount2 = Amount*10,
			{RoleAccID,RRType} = db_sql:get_role_accid_and_type(RoleID),
			case RRType of
				OurChannel ->
					pay_gold2(RoleID, Amount2, Receipt, Sign, OurChannel),
					Reply = ejson:encode({[{<<"result">>,1}, {<<"accid">>,RoleAccID}]}),
					Req:ok({"text/html; charset=utf-8", Reply});
				_->
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
    do_pay_from_quick(RoleID, Amount, Receipt, Sign, SrcType);
pay_gold2(RoleID, _Amount, Receipt, _Sign, _SrcType) ->
    ?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID, Receipt]),
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

verify_sign(QueryString)->
    Key = "04804669048465532396097073355615",
    Nt_data = proplists:get_value("nt_data",QueryString),
    Sign = proplists:get_value("sign", QueryString),
    Sign2 = decode(Sign,Key),
    Sign4 = md5("nt_data="++Nt_data),
    Sign5 = exchange(Sign4,[{1,13},{5,17},{7,23}]),
    case Sign2 =:= Sign5 of
        true ->
            Data = decode(Nt_data,Key),
            case get_info(Data) of
                {true,RoleID,Amount,OurChannel,OrderNo}->
                    {true,RoleID,Amount,OurChannel,OrderNo,md5(integer_to_list(RoleID)++integer_to_list(Amount)++integer_to_list(OurChannel)++OrderNo)};
                false ->
                    false
            end;
        false ->
            false
    end.

get_value(Response, Key) when is_binary(Key)->
    case lists:keyfind(Key, 1, Response) of
        false ->
            false;
        {Key, Value} ->
            Value
    end.

%加密
encode(Src,Key)->
    Src1 = encode1(Src,Key),
    Sign1 = md5("nt_data="++Src1),
    exchange(Sign1,[{1,13},{5,17},{7,23}]).

encode1(Src,Key) ->
    case lists:member(undefined,[Src,Key]) of
        true ->
            ?ERR("has null Src :~w Key: ~w ~n",[Src,Key]);
        false ->
            SrcLength = length(Src),
            KeyLength = length(Key),
            Temp = lists:seq(1,SrcLength),
            Result = lists:foldl(fun(Index,Acc0) ->
                DataElement = lists:nth(Index,Src),
                KeyElement = lists:nth(((Index-1) rem KeyLength)+1,Key),
                ResultElement = (DataElement band 16#ff) + (KeyElement band 16#ff),
                Acc0++"@"++integer_to_list(ResultElement)
                end,[],Temp),
            Result
    end.

%解密
decode(Src,Key) ->
    NewSrc = predeal(Src),
    % ?INFO("after predeal ~w ~n",[NewSrc]),
    NewSrcLength = length(NewSrc),
    case NewSrcLength >0 of
        true ->
            KeyLength = length(Key),
            Temp = lists:seq(1,NewSrcLength),
            ResultData1 = lists:foldl(fun(Index,Acc) ->
                DataElement = lists:nth(Index,NewSrc),
                KeyElement = lists:nth((((Index-1) rem KeyLength)+1),Key),
                NewDate = DataElement - (16#ff band KeyElement),
                [NewDate|Acc]
                end,[],Temp),
            lists:reverse(ResultData1);
        false ->
            ?ERR("NewSrc ~w length is 0 ~n",[NewSrc]),
            Src
    end.

%去除Src中的@字符
predeal(Src) ->
    case length(Src) > 0 of
        true ->
            List = string:tokens(Src,"@"),
            [list_to_integer(Element)||Element<-List];
        false ->
            []
    end.

%%根据交换规则处理字符串
exchange(Src,Rule) ->
    case is_list(Rule) of
        true ->
            lists:foldl(fun(RuleItem,Acc) ->
                exchange2(Acc,RuleItem)
            end,Src,Rule);
        false ->
            ?ERR("exchange failed"),
            Src
    end.

%%实际完成字符位置交换
exchange2(Src,{Index1,Index2}) ->
    Length = length(Src),
    case Index1 < Length andalso Index2 < Length andalso Index2 >=0 andalso Index1 >= 0 of
        true ->
            Char1 = lists:nth(Index1+1,Src),
            Char2 = lists:nth(Index2+1,Src),
            Temp = lists:seq(1,Length),
            ResultList = lists:foldl(fun(Index,Acc) ->
                    if Index == Index1+1 ->
                        [Char2|Acc];
                       Index == Index2+1 ->
                        [Char1|Acc];
                       true ->
                        [lists:nth(Index,Src)|Acc]
                    end
                end,[],Temp),
            lists:reverse(ResultList);
        false ->
            ?ERR("exchange2 fail Src:~p Index1:~w Index2:~w ~n",[Src,Index1,Index2])
    end.

md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%%提取xml中充值需要的数据
get_info(Data)-> 
    {ParsedDocumentRootElement, _RemainingText = ""} = xmerl_scan:string(Data),
    [{_,_,_,_,ChannelID,_}] = xmerl_xpath:string("//channel/text()",ParsedDocumentRootElement),
    [{_,_,_,_,Amount,_}] = xmerl_xpath:string("//amount/text()",ParsedDocumentRootElement),
    [{_,_,_,_,Status,_}] = xmerl_xpath:string("//status/text()",ParsedDocumentRootElement),
    [{_,_,_,_,ExtraInfo,_}] = xmerl_xpath:string("//extras_params/text()",ParsedDocumentRootElement),
    [{_,_,_,_,OrderNo,_}] = xmerl_xpath:string("//order_no/text()",ParsedDocumentRootElement),
    case Status of
        "0"->
            case quick_channel_to_our_channele2(ChannelID) of
                false ->
                    false;
                OurChannel->
                    [_,_,RoleID] = string:tokens(ExtraInfo,"A"),
                    {true,list_to_integer(RoleID),trunc(list_to_float(Amount)),OurChannel,OrderNo}
            end;
        "1"->
            false
    end.

%%将quick_sdk中定义的渠道号映射到我们游戏定义的渠道号
quick_channel_to_our_channele(?UNDEFIEND)-> ?ACCOUNT_TYPE_SHUNWANG;
quick_channel_to_our_channele(?C_SHUN_WANG)-> ?ACCOUNT_TYPE_SHUNWANG;
quick_channel_to_our_channele(?C_YUN_YOU)-> ?ACCOUNT_TYPE_YUNYOU;
quick_channel_to_our_channele(?C_HMPAY)-> ?ACCOUNT_TYPE_HM_ANDROID;
quick_channel_to_our_channele(?C_SINA) -> ?ACCOUNT_TYPE_SINA;
quick_channel_to_our_channele(?C_65) -> ?ACCOUNT_TYPE_65;
quick_channel_to_our_channele(?C_TENCENT)-> ?ACCOUNT_TYPE_QUICK_YYB;
quick_channel_to_our_channele(?C_LEMENGHUDONG)-> ?ACCOUNT_TYPE_QUICK_LEMENGHUDONG;
quick_channel_to_our_channele(?C_49YOU)->?ACCOUNT_TYPE_49YOU;
quick_channel_to_our_channele(ChannelID) ->
    ?ERR("undefined ChannelID: ~w ~n",[ChannelID]),
    false.

quick_channel_to_our_channele2(QuickChannel)->
    case data_quick:get(quick_channel_map) of
        ?undefined->
            ?ERR("can not find channel map in data_common.config~n"),
            false;
        List ->
            case lists:keyfind(QuickChannel,1,List) of
                false->
                    ?ERR("undefined quick channel:~p ~n",[QuickChannel]),
                    false;
                {_QuickChannel,OurChannel,_AccidChannel,_Prefix}->
                    OurChannel
            end
    end.

do_pay_from_quick(RoleID, Amount, Receipt, Sign, SrcType)->
    case SrcType of
        ?ACCOUNT_TYPE_SHUNWANG ->
            pay_server:do_pay_from_shunwang(RoleID, Amount, Receipt, Sign, SrcType);
        ?ACCOUNT_TYPE_YUNYOU ->
            pay_server:do_pay_from_yunyou(RoleID, Amount, Receipt, Sign, SrcType);
        ?ACCOUNT_TYPE_HM_ANDROID ->
            pay_server:do_pay_from_hm_android(RoleID, Amount, Receipt, Sign, SrcType);
        ?ACCOUNT_TYPE_SINA ->
            pay_server:do_pay_from_sina(RoleID, Amount, Receipt, Sign, SrcType);
		?ACCOUNT_TYPE_65 ->
			pay_server:do_pay_from_65(RoleID,Amount,Receipt, Sign,SrcType);
        ?ACCOUNT_TYPE_QUICK_YYB ->
            pay_server:do_pay_from_quick(RoleID,Amount,Receipt, Sign,SrcType);
        ?ACCOUNT_TYPE_QUICK_LEMENGHUDONG ->
            pay_server:do_pay_from_quick(RoleID,Amount,Receipt, Sign,SrcType);
        ?ACCOUNT_TYPE_49YOU->
            pay_server:do_pay_from_quick(RoleID,Amount,Receipt,Sign,SrcType);
        _ ->
            pay_server:do_pay_from_quick(RoleID,Amount,Receipt,Sign,SrcType)
    end.
