%% @author : lixinglong
%% @doc : web mod

-module(mail_gift_mod).
-export([mail_gift/2]).
-include("def_role.hrl").
-include("def_item.hrl").
%%操作类型=========================================%%
-define(FIX_ROLE_VIP,"8").
-define(ILLEGAL_REWARD,"9").
-define(GOLD2GOLDBONUS_TYPE,"10").
-define(SILENT_USER_DEAL_TYPE,"11").

%%操作结果=========================================%%
-define(ROLE_NO_EXIST_IN_SERVER,"1").
-define(ROLE_IN_SERVER_SUCCESS,"2").
-define(ILLEGAL_TIME,"3").
%% type: 0:全体玩家  1:列表内的玩家  2:全体在线玩家  3:指定等级玩儿家 4:指定vip范围的玩家 5:VIP and level and 最后登录时间范围 其他:忽略

mail_gift(Req, _DocRoot)->
    %?INFO("mail_gift11 ~w",[Req]),
	case parse_Req(Req) of 
		{ok, Type1,Message, Users, Rewards, Items, Gers,Range,Vip} -> 
            ?INFO("mail_gift ~w",[{Type1,Message, Users, Rewards, Items, Gers,Range,Vip}]),
			{Reward,Type} = get_rewards(Rewards, Items, Gers,Type1),
			case Type of
				"0" ->
					role_mail_gift:send_everyone_reward(Reward, Message, 0, []),
					Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
				"1" ->
					if Users =:= [] ->
						   ok;
					   true->
						   UserList = get_users(Users),
						   role_mail_gift:send_gift_by_name(UserList, Reward, Message)
					end,
					Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
				"2" ->
					role_mail_gift:send_onliners_gift(Reward, Message),
					Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
				"3" ->
					%% OfftimeHigh是指最晚登陆时间
					{[{_,LevelLow},{_,LevelHigh},{_,OfftimeHigh}]} = ejson:decode(Range),
					case trans(OfftimeHigh) of 
						 OfflineTime when is_integer(OfflineTime) andalso OfflineTime > 0 ->
							role_mail_gift:send_level_offtime_reward(Reward, Message, trans(LevelLow), trans(LevelHigh),0, OfflineTime, 0, []);
						_ ->
							role_mail_gift:send_level_reward(Reward, Message, trans(LevelLow), trans(LevelHigh))
					end,
					Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
				"4" ->
					{[{_,VipLow},{_,VipHigh},{_,OfftimeHigh}]} = ejson:decode(Range),
					case trans(OfftimeHigh) of 
						 OfflineTime when is_integer(OfflineTime) andalso OfflineTime > 0 ->
							role_mail_gift:send_vip_offtime_reward(Reward, Message, trans(VipLow), trans(VipHigh),0, OfflineTime);
						_ ->
							role_mail_gift:send_vip_reward(Reward, Message, trans(VipLow), trans(VipHigh))
					end,
					Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
                "5" ->
                    {[{_,LevelLow},{_,LevelHigh},{_,VipLow},{_,VipHigh},{_,OfftimeHigh}]} = ejson:decode(Range),
                    ?INFO("mail_gift ~w ~w ~w ~w ~w",[LevelLow,LevelHigh,VipLow,VipHigh,OfftimeHigh]),
                    role_mail_gift:send_level_vip_offtime_reward(Reward, Message, trans(LevelLow), trans(LevelHigh),
                                                         trans(VipLow), trans(VipHigh),0, trans(OfftimeHigh)),
                   	Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
				"6" ->
                    {[{_,LevelLow},{_,LevelHigh},{_,VipLow},{_,VipHigh},{_,Offtime2},{_,Offtime},{_,SrcType}]} = ejson:decode(Range),  %  Offtime unix时间戳 Low < High  Off2 < Off
					role_mail_gift:send_type_level_and_vip_role_reward(Reward, Message, trans(LevelLow), trans(LevelHigh), trans(VipLow), trans(VipHigh), trans(SrcType), trans(Offtime),trans(Offtime2), 0, []),
                	Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
                "7" ->
                    {[{_,LevelLow0a},{_,LevelHigh0a},{_,VipLow0a},{_,VipHigh0a},{_,Offtime2_0a},{_,Offtime_0a},{_,SrcType}]} = ejson:decode(Range),  %  Offtime unix时间戳 Low < High  Off2 < Off
                    
                    LevelLow0 = trans(LevelLow0a),
                    LevelHigh0 = trans(LevelHigh0a),
                    VipLow0 = trans(VipLow0a),
                    VipHigh0 = trans(VipHigh0a),
                    Offtime2_0 = trans(Offtime2_0a),
                    Offtime_0 = trans(Offtime_0a),
                    
                    LevelLow = if LevelLow0 < 0 ->
                                      0;
                                  true ->
                                      LevelLow0
                               end,
                    LevelHigh = if LevelHigh0 < 0 ->
                                      9999;
                                  true ->
                                      LevelHigh0
                               end,
                    VipLow = if VipLow0 < 0 ->
                                      0;
                                  true ->
                                      VipLow0
                               end,
                    VipHigh = if VipHigh0 < 0 ->
                                      99;
                                  true ->
                                      VipHigh0
                               end,
                    Offtime = if Offtime_0 < 0 ->
                                      2147483647;
                                  true ->
                                      Offtime_0
                               end,
                    Offtime2 = if Offtime2_0 < 0 ->
                                      0;
                                  true ->
                                      Offtime2_0
                               end,
                    ?INFO("mail_gift type-7 ~w ~w ~w ~w ~w ~w ~w --------- ~w ~w ~w ~w ~w ~w ~w",[LevelLow0,LevelHigh0,VipLow0,VipHigh0,Offtime2_0,Offtime_0,SrcType
                                                            ,(LevelLow), (LevelHigh), (VipLow), (VipHigh), trans(SrcType), (Offtime),(Offtime2)]),
                    spawn(fun()->role_mail_gift:send_type_level_and_vip_role_reward(Reward, Message, (LevelLow), (LevelHigh), (VipLow), (VipHigh), trans(SrcType), (Offtime), (Offtime2), 0, []) end),
                    Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
                 ?FIX_ROLE_VIP->
                 	case check_vip(Vip) of
                 		{true,VipLevel}->
                 			InfoList = get_vip_info(VipLevel,Users),
                 			?INFO("InfoList:~w ~n",[InfoList]),
                 			role_mail_gift:set_role_vip(InfoList),
                 			Reply = ejson:encode({[{<<"result">>,<<"succ">>}]});
                 		false->
                 			Reply = ejson:encode({[{<<"result">>,<<"illegal_vip_level">>}]})
                 	end;

                 ?ILLEGAL_REWARD->
                 	Reply = ejson:encode({[{<<"result">>,<<"illegal_reward">>}]});
                 _ ->
                 	Reply = ejson:encode({[{<<"result">>,<<"undefined_type">>}]}),
					ignore
			end,
			Req:ok({"text/html; charset=utf-8", Reply});
		_ -> 
			Reply = ejson:encode({[{<<"result">>,<<"err">>}]}),
			Req:ok({"text/html; charset=utf-8", Reply})
	end.

handle(Req,_DocRoot)->
	Result = case parse_Req2(Req) of 
		{ok,Type,Message}->
			if
				Type=:=?GOLD2GOLDBONUS_TYPE ->
					do_transform_gold2goldbonus(Message,[]);
				Type=:=?SILENT_USER_DEAL_TYPE->
					do_silent_user_deal(Message,[]);
				true->
					?ERR("Type:~w Message:~w ~n",[Type,Message]),
					<<"undefined type">>
			end;
		_->
			?ERR("processinfo:~w ~n",[process_info(self())]),
			receive 
				a->
					ok
				after 10000->
					ok
			end,
			<<"error">>
	end,
	Reply = ejson:encode({[{<<"result">>,Result}]}),
	Req:ok({"text/html; charset=utf-8", Reply}).

trans(V) ->
    erlang:list_to_integer(erlang:binary_to_list(V)).

parse_Req(Req)->
	QueryString = Req:parse_post(),
	%% 提取消息中的信息
	?INFO("QueryString:~w ~n",[QueryString]),
	Pass = proplists:get_value("pass", QueryString),
	Message = proplists:get_value("message", QueryString),
	{[{_,MessageT}]} = ejson:decode(Message),
	Message1 = binary:bin_to_list(MessageT),
	UserList = proplists:get_value("userlist", QueryString),
	RewardList = proplists:get_value("reward", QueryString),
	ItemList = proplists:get_value("item", QueryString),
	GerList = proplists:get_value("ger", QueryString),
	Type = proplists:get_value("type", QueryString),
	Vip = proplists:get_value("vip",QueryString),
	Range = proplists:get_value("range", QueryString),
	{can_pass(Pass, Message1),Type, Message1, UserList, RewardList, ItemList, GerList, Range,Vip}.

can_pass(Pass, Message)	->
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth, _, _} = data_setting:get(passinfo),
	Auth3 = util:md5(lists:append(Auth, Message)),
	
	if Pass =:= Auth3 -> ok;
	   true -> failed  
	end.

parse_Req2(Req)->
	QueryString = Req:parse_post(),
	%% 提取消息中的信息
	Pass = proplists:get_value("pass", QueryString),
	Message = proplists:get_value("message", QueryString),
	Type = proplists:get_value("type",QueryString),
	{can_pass(Pass, Message,Type),Type,parse_message(Message,Type)}.

can_pass(Pass, Message,Type)	->
	% ?ERR("Pass:~w Message:~w ~n",[Pass,Message]),
	%% 认证消息是否有效,认证失败,则消息不进行公告广播
	{Auth,_Port,_IP}= data_setting:get(passinfo),
	Auth3 = util:md5(Auth),
	if Pass =:= Auth3 -> ok;
	   true -> failed  
	end.

get_users(T) ->
	mochijson2:decode(T).

%%此处返回sell_reward|ILLEGAL_REWARD
get_rewards(Reward, Item, Ger,Type)->
    ?INFO("get_rewards ~w ~w ~w",[Reward, Item, Ger]),
	Rewards = parse_reward(Reward),
	Items = parse_item(Item), 
	Gers = parse_ger(Ger),
	[Coin, Gold, Repu, Exp] = Rewards,
	SellReward1 = #sell_reward{coin=Coin,roleExp=Exp,gerExp=0,gold=Gold,reputation=Repu},
	case check_reward(Items,Gers) of
		true->
			{SellReward1#sell_reward{item=Items,newGer=Gers},Type};
		false->
			{#sell_reward{},?ILLEGAL_REWARD}
	end.
	% {sell_reward,Coin, Exp,0, Gold, Items, Repu, Gers}.

check_reward(Items,Gers)->
	ItemResult = check_item(Items),
	GerResult = check_ger(Gers),
	?INFO("ItemResult:~w GerResult:~w ~n",[ItemResult,GerResult]),
	ItemResult andalso GerResult.

check_ger(Gers)->
	lists:all(fun(Ger)->
		case data_ger:get(Ger#new_ger.gerTypeID) of
			?undefined->
				?ERR("undefined GerTypeID:~w Gers:~w ~n",[Ger#new_ger.gerTypeID,Gers]),
				false;
			_ ->
				Ger#new_ger.gerQuality =< ?MAX_GER_SECOND_RANK andalso Ger#new_ger.gerQuality >= 0  andalso Ger#new_ger.gerLevel > 0 andalso Ger#new_ger.gerLevel =< data_common:get(max_ger_level)
		end
	end,Gers).
	
check_item(Items)->
	lists:all(fun(Item1)->
		Item = item_lib:transfor_newitem2newitem2(Item1),
		case data_item:get(Item#new_item2.itemTypeID) of
			?undefined->
				?ERR("undefined ItemTypeID:~w ~n",[Item#new_item2.itemTypeID]),
				false;
			#data_item{itemType=ItemType,itemMaxRank=MaxRank}->
				IsDiamond = item_lib:is_skill_diamond(ItemType),
				if
				 	ItemType =:= ?other orelse ItemType =:= ?material orelse ItemType =:= ?soul_general orelse ItemType =:= ?debris_weapon 
					orelse ItemType =:= ?debris_armor orelse ItemType =:= ?debris_horse orelse ItemType =:= ?box orelse ItemType =:= ?formula orelse ItemType =:= ?add_times orelse IsDiamond->
				 		check_normal_item(Item,MaxRank);
				 	ItemType =:= ?stonechip->
				 		check_stonechip(Item,MaxRank);
				 	true->
				 		check_equip(Item,MaxRank,ItemType)
				end
		end
	end,Items).

check_equip(Item,MaxRank,ItemType)->
	#new_item2{itemRank=ItemRank,enchantType=EnchantType,enchantLevel=EnchantLevel,itemLevel=ItemLevel} = Item,
	case lists:member(ItemType,?EQUIP_TYPE_LIST_NO_STONE) of 
		true->
			ItemRank >= 0 andalso ItemRank =< MaxRank andalso lists:member(EnchantType,[0|?ENCHANT_TYPE]) andalso EnchantLevel >=0 andalso EnchantLevel =< data_item_enchant:get(max_enchant_level) andalso ItemLevel >0;
		false->
			case lists:member(ItemType,?TRAINER_EQUIP_TYPE) of
				true->
					ItemRank >= 0 andalso ItemRank =< MaxRank andalso EnchantType =:= 0 andalso EnchantLevel =:=0 andalso ItemLevel>0;
				false->
					case lists:member(ItemType,?ESSENCE_STONE_TYPE++?STONE_TYPE++?STONE_LEGEND_LIST) of
						true->
							 ItemRank >= 0 andalso ItemRank =< 10 andalso EnchantType =:= 0 andalso EnchantLevel =:=0 andalso ItemLevel>0 andalso ItemLevel =<10;
						false->
							case item_lib:is_accelerate_equip(ItemType) of
								true->
									ItemRank >= 0 andalso ItemRank =< MaxRank andalso EnchantType =:= 0 andalso EnchantLevel =:=0 andalso ItemLevel>0;
								false->
									false
							end
					end
			end
	end.
check_stonechip(Item,_MaxRank)->
	#new_item2{itemRank=ItemRank,itemLevel=ItemLevel,enchantType=EnchantType,enchantLevel=EnchantLevel} = Item,
	ItemRank =:=0 andalso ItemLevel =:=1 andalso EnchantType=:=0 andalso EnchantLevel =:=0.

check_normal_item(Item,_MaxRank)->
	#new_item2{itemRank=ItemRank,itemLevel=ItemLevel,enchantType=EnchantType,enchantLevel=EnchantLevel} = Item,
	ItemRank =:= 0 andalso ItemLevel =:= 1 andalso EnchantType=:=0 andalso EnchantLevel=:=0.

check_vip(Vip) when is_integer(Vip)->
	case data_vip:get(Vip) of
		?undefined->
			?ERR("undefined vip:~w ~n",[Vip]),
			false;
		 _->
		 	{true,Vip}
	end;

check_vip(Vip) ->
	case Vip of
		?undefined->
			?ERR("vip is undefined~n"),
			false;
		_ ->
			check_vip(list_to_integer(Vip))
	end.

parse_reward(Reward) ->
	List = lists:flatten(tuple_to_list(ejson:decode(Reward))),
	lists:map(fun(X)->{_,B} = X,
					  B
			  end,List).


parse_item(Item) when Item =:= ?undefined->
    [];
parse_item(Item)->
	List = ejson:decode(Item),
	ItemList = lists:foldl(fun(B,Acc)->
		[D] = tuple_to_list(B),
		ItemTypeID = proplists:get_value(<<"itemTypeID">>,D),
		ItemNum = proplists:get_value(<<"itemNum">>,D),
		ItemLevel = proplists:get_value(<<"itemLevel">>,D),
		ItemRank = proplists:get_value(<<"itemRank">>,D),
		EnchantType = proplists:get_value(<<"enchantType">>,D),
		EnchantLevel = proplists:get_value(<<"enchantLevel">>,D),
		case data_item:get(ItemTypeID) of
			?undefined->
				?ERR("undefined ItemTypeID:~w Item:~w ~n",[ItemTypeID,Item]),
				Acc;
			_ ->
				[#new_item2{itemTypeID=ItemTypeID,itemNum=ItemNum,itemLevel=ItemLevel,itemRank=ItemRank,enchantType=EnchantType,enchantLevel=EnchantLevel}|Acc]
		end
	end,[],List),
	ItemList.


parse_ger(Ger) when Ger =:= ?undefined->
    [];
parse_ger(Ger)->
	List = ejson:decode(Ger),
	lists:foldl(fun(B,Acc)->
		[D] = tuple_to_list(B),
		GerTypeID = proplists:get_value(<<"gerTypeID">>,D),
		GerNum = proplists:get_value(<<"gerNum">>,D),
		GerLevel = proplists:get_value(<<"gerLevel">>,D),
		GerQuality = proplists:get_value(<<"gerQuality">>,D),
		case data_ger:get(GerTypeID) of
			?undefined->
				?ERR("undefined GerTypeID:~w Ger:~w ~n",[GerTypeID,Ger]),
				Acc;
			_ ->
				Single = #new_ger{gerTypeID=GerTypeID,gerLevel=GerLevel,gerQuality=GerQuality},
				TempGerList = lists:duplicate(GerNum,Single),
				TempGerList ++ Acc
		end
	end,[],List).

get_vip_info(Vip,Users) when is_integer(Vip)->
	UserList = get_users(Users),
	RoleIDList = lists:foldl(fun(UserName,Acc)->
		case db_sql:search_roleName2(util:latin1(UserName)) of
			[AccountID,RoleID] when is_integer(AccountID)->
				[RoleID|Acc];
			_ ->
				?ERR("not find RoleID UserName:~w ~n",[UserName]),
				Acc
		end
	end,[],UserList),
	[#vip_info{roleid=RoleID,vip=Vip}||RoleID<-RoleIDList];

get_vip_info(Vip,Users)->
	case Vip of
		?undefined->
			?ERR("VipInfo:~w ~n",[Vip]),
			[];
		_ ->
			get_vip_info(list_to_integer(Vip),Users)
	end.

parse_message(Message,Type)->
	if
		Type=:=?GOLD2GOLDBONUS_TYPE ->
			parse_gold2goldbonus_message(Message);
		Type=:=?SILENT_USER_DEAL_TYPE->
			parse_slient_user_deal_message(Message);
		true->
			false
	end.

parse_gold2goldbonus_message(Message)->
	L = ejson:decode(Message),
	parse_gold2goldbonus_message2(L,[]).

parse_gold2goldbonus_message2([],Acc)->
	Acc;
parse_gold2goldbonus_message2([{H}|T],Acc)->
	AccountID = proplists:get_value(<<"accountID">>,H),
	OldType = proplists:get_value(<<"oldType">>,H),
	NewType = proplists:get_value(<<"newType">>,H),
	parse_gold2goldbonus_message2(T,[{AccountID,OldType,NewType}|Acc]).

parse_slient_user_deal_message(Message)->
	{L} = ejson:decode(Message),
	parse_slient_user_deal_message2(L).

parse_slient_user_deal_message2(Message)->
	RoleIDList = proplists:get_value(<<"roleidlist">>,Message),
	BeginTimeStamp = proplists:get_value(<<"begintimestamp">>,Message),
	EndTimeStamp = proplists:get_value(<<"endtimestamp">>,Message),
	[{RoleID,BeginTimeStamp,EndTimeStamp}||RoleID<-RoleIDList].

%%将Message玩家的gold转换成goldbonus
do_transform_gold2goldbonus(false,Result)->
	<<"error">>;
do_transform_gold2goldbonus([],[])->
	<<"success">>;
do_transform_gold2goldbonus([],Result)->
	list_to_binary(Result++"]");
do_transform_gold2goldbonus([{AccountID,OldType,NewType}|T],Result)->
	ServerID = data_setting:get(server_id),
	Message = case do_transform_gold2goldbonus2(AccountID,OldType,NewType) of
		false->
			"{\"AccountID\":"++integer_to_list(AccountID)++",\"ServerID:\":"++integer_to_list(ServerID)++",\"Result\":"++?ROLE_NO_EXIST_IN_SERVER++"}";
		RoleIDList ->
			RoleIDList1 = string:join([integer_to_list(RoleID)||RoleID<-RoleIDList],","),
			"{\"AccountID\":"++integer_to_list(AccountID)++",\"ServerID:\":"++integer_to_list(ServerID)++",\"Result\":"++?ROLE_IN_SERVER_SUCCESS++",\"roleID\":["++RoleIDList1++"]}"
	end,
	case Result of
		[]->
			do_transform_gold2goldbonus(T,"["++Message);
		_ ->
			do_transform_gold2goldbonus(T,Result++","++Message)
	end.

%%此处如果将OldType和NewType写成玩家当前的渠道Type则会实现将玩家的gold转换成goldbonus,由于存在合服的情况，被合服的区服ID不会传递过来，故需要计算包括合服之后账号的ID
do_transform_gold2goldbonus2(AccountID,OldType,NewType)->
	case is_platform_accountid_exist_on_server(AccountID) of
		{true,RoleIDList}->
			lists:foreach(fun(RoleID)->
							case catch role_lib:send_server(RoleID, {gold2goldbonus,{OldType,NewType,?MONEY_DEC_TYPE_SRCTYPE_TRANSFORM,AccountID,0}}) of
								{'EXIT',_}->
                    				db_sql:gold2goldbonus(RoleID,OldType,NewType,?MONEY_DEC_TYPE_SRCTYPE_TRANSFORM,AccountID);
								_ ->
									ok
							end
						end,RoleIDList),
			RoleIDList;
		false->
			false
	end.
do_silent_user_deal([],Result)->
	List = ["{\"code\":"++Type++",\"roleids\":["++List++"]}"||{Type,List}<-Result],
	list_to_binary("["++string:join(List,",")++"]");
do_silent_user_deal([{RoleID,_BeginTime,_EndTime}=H|T],Result)->
	{ResultType,RoleList}=Msg= case do_silent_user_deal2(H) of
		false->
			{?ROLE_NO_EXIST_IN_SERVER,[integer_to_list(RoleID)]};	
		{false,?ILLEGAL_TIME}->
			{?ILLEGAL_TIME,[integer_to_list(RoleID)]};
		true->
			{?ROLE_IN_SERVER_SUCCESS,[integer_to_list(RoleID)]}
	end,
	case lists:keytake(ResultType,1,Result) of
		false->
			do_silent_user_deal(T,[Msg|Result]);
		{_Value,{FindType,FindRoleList},Other}->
			do_silent_user_deal(T,[{FindType,RoleList++","++FindRoleList}|Other])
	end.

do_silent_user_deal2({RoleID,BeginTimeStamp,EndTimeStamp})->
	{Result,LogTimeStamp} = get_random_log_time(BeginTimeStamp,EndTimeStamp),
	case Result of
		true->
			case db_sql:get_role_accid_and_type(RoleID) of
				{0,0}->
					false;
				{AccountID,SrcType}->
					case catch role_lib:send_server(RoleID, {gold2goldbonus,{SrcType,SrcType,?MONEY_DEC_TYPE_SPECIAL,AccountID,LogTimeStamp}}) of
						{'EXIT',_}->
                    		db_sql:gold2goldbonus(RoleID,SrcType,SrcType,?MONEY_DEC_TYPE_SPECIAL,AccountID,LogTimeStamp);
						_ ->
							ok
					end,
					true
			end;
		false->
			{false,?ILLEGAL_TIME}
	end.
%%检查平台accountID是否在当前区服有角色
is_platform_accountid_exist_on_server(AccountID)->
	AllServerID = [data_setting:get(server_id)|data_setting:get(merge_server_id_list)],
	RoleIDList = lists:foldl(fun(ServerID,Acc)->
		Accid = (ServerID + 1) * ?AccidBase + AccountID,
		case db_sql:check_roleCreated(Accid) of
			false->
				Acc;
			{true,RoleID}->
				[RoleID|Acc]
		end
	end,[],AllServerID),
	case RoleIDList=:=[] of
		true->
			false;
		false->
			{true,RoleIDList}
	end.

%%此处必须begintimestamp和endtimestamp处于当前月，才能生效
get_random_log_time(BeginTimeStamp,EndTimeStamp) when is_integer(BeginTimeStamp) andalso is_integer(EndTimeStamp)->
	case check_time_illegal(BeginTimeStamp,EndTimeStamp) of
		true->
			random:seed(now()),
			Time = if
				BeginTimeStamp<EndTimeStamp ->
					BeginTimeStamp+random:uniform(EndTimeStamp-BeginTimeStamp);
				BeginTimeStamp>EndTimeStamp->
		    		EndTimeStamp+random:uniform(BeginTimeStamp-EndTimeStamp);
				true->
		    		BeginTimeStamp
			end,
			{true,Time};
		false->
			{false,?ILLEGAL_TIME}
	end.
check_time_illegal(BeginTimeStamp,EndTimeStamp)->
	{{BYear,BMon,_BDay},_} = util:seconds_to_datetime(BeginTimeStamp),
	{{EYear,EMon,_EDay},_} = util:seconds_to_datetime(EndTimeStamp),
	{CYear,CMon,_CDay} = erlang:date(),
	BYear=:=EYear andalso BYear=:=CYear andalso BMon=:=EMon andalso BMon=:=CMon.
%-------------------------------------------------------------------------------------------------------------
	
web_test()	->
	inets:start(),
	UserList=ejson:encode({[{<<"1">>,<<"冯晓倩">>}
							,{<<"2">>,<<"ddd">>}
							,{<<"3">>,<<"孔文莹">>}
						   ]}),
	Users = http_uri:encode(binary:bin_to_list(UserList)),
	Msg1 = ejson:encode({[{<<"msg">>,<<"今天天气不错">>}]}),
	{[{_,MsgT}]}=ejson:decode(Msg1),
	Message = http_uri:encode(binary:bin_to_list(Msg1)),
	%Pass=erlang:md5(lists:merge("passed", binary:bin_to_list(Msg1))),
	Pass = util:md5(lists:merge("passed", binary:bin_to_list(MsgT))),
	Type = "2",
	%RewardList = ejson:encode({[{<<"gold">>,<<"0">>}%coin
	%							,{<<"coin">>,<<"0">>}%gold
	%							,{<<"renown">>,<<"0">>}%repu
	%							,{<<"experience">>,<<"1">>}%exp
	%						   ]}),
		RewardList = ejson:encode({[{<<"gold">>,0}
								,{<<"coin">>,0}
								,{<<"renown">>,0}
								,{<<"experience">>,1}
							   ]}),
	Reward = http_uri:encode(binary:bin_to_list(RewardList)),
	%ItemList = ejson:encode({[{<<"1">>,<<"10">>}
	%					  ,{<<"11001">>,<<"10000">>}
	%					 ]}),
		ItemList = ejson:encode({[{<<"1">>,10}
						  ,{<<"11001">>,30}
						 ]}),
	Item = http_uri:encode(binary:bin_to_list(ItemList)),
	%GerList = ejson:encode({[{<<"12403">>,<<"1">>}
	%						 ,{<<"12402">>,<<"2">>}]}),
	GerList = ejson:encode({[]}),%{<<"12403">>,1}
							 %,{<<"12402">>,2}]}),
	Ger = http_uri:encode(binary:bin_to_list(GerList)),
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s&userlist=~s&reward=~s&item=~s&ger=~s", 
									  [Pass,Message, Type, Users, Reward,Item,Ger])),
	%io:format("~w\n{p_action",[Arg]),
	httpc:request(post, {"http://127.0.0.1:8088/mailgift",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).
web_test2()	->
	inets:start(),
	Range = http_uri:encode(binary:bin_to_list(ejson:encode({[{<<"1">>,<<"1">>},{<<"1">>,<<"300">>},{<<"1">>,<<"1">>},{<<"1">>,<<"20">>},{<<"1">>,<<"1414389615">>},{<<"1">>,<<"1419429361">>},{<<"1">>,<<"3">>}]}))),
	UserList=ejson:encode({[{<<"1">>,<<"冯晓倩">>}
							,{<<"2">>,<<"ddd">>}
							,{<<"3">>,<<"孔文莹">>}
						   ]}),
	Users = http_uri:encode(binary:bin_to_list(UserList)),
	Msg1 = ejson:encode({[{<<"msg">>,<<"今天天气不错">>}]}),
	{[{_,MsgT}]}=ejson:decode(Msg1),
	Message = http_uri:encode(binary:bin_to_list(Msg1)),
	Pass = util:md5(lists:merge("passed", binary:bin_to_list(MsgT))),
	Type = "6",

		RewardList = ejson:encode({[{<<"gold">>,0}
								,{<<"coin">>,0}
								,{<<"renown">>,0}
								,{<<"experience">>,1}
							   ]}),
	Reward = http_uri:encode(binary:bin_to_list(RewardList)),
		ItemList = ejson:encode({[{<<"1">>,10}
						  ,{<<"11001">>,30}
						 ]}),
	Item = http_uri:encode(binary:bin_to_list(ItemList)),

	GerList = ejson:encode({[{<<"12401">>,3},{<<"12402">>,3},{<<"12403">>,3}]}),
	Ger = http_uri:encode(binary:bin_to_list(GerList)),
	Arg = lists:flatten(io_lib:format("pass=~s&message=~s&type=~s&userlist=~s&reward=~s&item=~s&ger=~s&range=~s", 
									  [Pass,Message, Type, Users, Reward,Item,Ger,Range])),
	io:format("~w\n{p_action",[Arg]),
	httpc:request(post, {"http://10.10.11.11:8059/mailgift",
						 [], "application/x-www-form-urlencoded",Arg}, [], []).

