%% @author lxl
%% @doc @todo Add description to pay_mod_91.


-module(pay_mod_zd).
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pay_gold/1]).

pay_gold(Req) ->
    QueryString = Req:parse_qs(),
    %?ERR("QS=~p\n",[QueryString]),

    GameKey = proplists:get_value("game_key", QueryString),
    GameOrderNo = proplists:get_value("game_orderno", QueryString),
    Nonce = proplists:get_value("nonce", QueryString),
    Subject = proplists:get_value("subject", QueryString),
    TimeStamp = proplists:get_value("timestamp", QueryString),
    TotalFee = proplists:get_value("total_fee", QueryString),
    Sign = proplists:get_value("signature", QueryString),

    
    Amount = trunc(erlang:list_to_float(TotalFee) * 10),
    
    SignString = "game_key="++GameKey++"&game_orderno="++GameOrderNo++"&nonce="++Nonce++"&subject="++
                     Subject++"&timestamp="++TimeStamp++"&total_fee="++TotalFee++"&08AAB76DBBAF4176892E09390DB05422",
    
    [ServerID,RoleID|_] = string:tokens(GameOrderNo,"A"),
    RoleID2 = list_to_integer(RoleID),
    LocalSign = sign([SignString]),
    if Sign == LocalSign ->
           RoleID2 = list_to_integer(RoleID),
           {RoleAccID,SrcType} = db_sql:get_role_accid_and_type(RoleID2),
           case SrcType of
               2 ->
                   Reply = ejson:encode({[{<<"result">>,1},{<<"accid">>,RoleAccID}]}),
                   Req:ok({"text/html; charset=utf-8", Reply}),
                   QS = mochiweb_util:urlencode(QueryString),
                   pay_gold2(RoleID2,Amount,QS,Sign	,121);
               _ ->
                   ?ERR("type error ~w",[SrcType]),
                   Reply = ejson:encode({[{<<"result">>,0}]}),
                   Req:ok({"text/html; charset=utf-8", Reply})
           end;
       true ->
           Reply = ejson:encode({[{<<"result">>,0}]}),
           Req:ok({"text/html; charset=utf-8", Reply})
    end.



pay_gold2(RoleID,Amount,Req,Sign,SrcType) when is_integer(RoleID) ->
	pay_server:do_pay_from_91(RoleID,Amount,Req,Sign,SrcType);
pay_gold2(RoleID,_Amount,Req,_Sign,_SrcType) ->
	?ERR("pay_gold2 err: roleid:~w,req:~w\n",[RoleID,Req]),
	[].
%% ====================================================================
%% Internal functions
%% ====================================================================

sign(StrList) ->
	md5(sign2(StrList)).

sign2([A]) ->
	A;
sign2([A,B]) ->
	A++B;
sign2([]) ->
	"";
sign2([A|T]) ->
	A++sign2(T).

md5(S) ->
	lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).
