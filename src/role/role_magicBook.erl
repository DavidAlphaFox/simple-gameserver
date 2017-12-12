-module(role_magicBook).
-compile(export_all).
-include("def_role.hrl").

%%TODO 跨服数据的升级(写到更新说明里面去)
%% 魔典
%% 0 (服务器不用,发给客户端时使用) 1 解锁 2 收集,但没满 3 收集满了 9 领取奖品了(当前解锁到第几个由状态字符串的长度决定)
%% 图鉴
%% 0 未收集 1 位置1 2 位置2 3位置3  9 领取奖品了

%%%============================================================================
cs_magicBook_swallow_ger(#cs_magicBook_swallow_ger{bookID=BookID,order=Order,gerID=GerID,pos=Pos}) ->
    {_, Code} = swallow_ger(BookID, Order, GerID, Pos),
    ?sendself(#sc_magicBook_swallow_ger{result=Code}),
    %% 通知客户端更新协议
    send_magicBook_book_detial(BookID).

cs_magicBook_summary(_) ->
    #magicBook_state{summary=BookStr, percent=PercentList} = role_data:get_magicBook_state(),
    Summary = gen_magicBook_summary(1, BookStr, PercentList, []),
    ?sendself(#sc_magicBook_summary{bookState=Summary}).

cs_magicBook_picture_reward(#cs_magicBook_picture_reward{bookID=BookID, order=Order}) ->
    {_, Code} = get_picture_reward(BookID, Order),
    ?sendself(#sc_magicBook_picture_reward{result=Code}).

cs_magicBook_book_reward(#cs_magicBook_book_reward{bookID=BookID}) ->
    {_, Code} = get_book_reward(BookID),
    ?sendself(#sc_magicBook_book_reward{result=Code}).

cs_magicBook_book_info(#cs_magicBook_book_info{bookID=BookID}) ->
    #magicBook_state{summary=Summary, picture_state=PictureState} = role_data:get_magicBook_state(),
    {BookState, PictureStr} = 
        case util:safe_nth(BookID, Summary) of
            null ->
                {$0, ""};
            Any ->
                {Any, lists:nth(BookID, PictureState)}
        end,
    Buffer = calc_magicBook_add_attr(BookID, BookState, PictureStr),
    ?sendself(#sc_magicBook_book_info{attr=Buffer, state=PictureStr}).

%%%============================================================================
%%%初始化
%% 开启条件判断
check_open(#role{level=Level}) ->
    Level >= data_magic_book:get(open_level).

%% 宝典开启判断
check_open_book(BookID) ->
    #role{level=Level} = role_data:get_roleInfo(),
    #magic_book{open_level=OpenLevel} = data_magic_book:get({book, BookID}),
    Level >= OpenLevel.

%% 货币类消耗
check_money(_,[],_) ->
    true;
check_money(Role,[{Type,Need}|MT],[Code|CT]) ->
    case role_lib:check_money(Role, Type, Need) of 
        true ->
            check_money(Role, MT, CT);
        _ ->
            {false, Code} 
    end.

%% 物品判断 
check_item([], _) ->
    void;
check_item(NeedItemList, Code) ->
    Items = role_data:get_bagItem(),
    ResultInfo = 
        lists:foldl(fun({new_item,ItemTypeID,Num,_,_},{AccList,AccResult,DelItemAcc,UpdataItemAcc,UpdateItemLogAcc})->
                       case AccResult of
                           true ->
                              case item_lib:check_material2(AccList, ItemTypeID, Num) of
                                  {BagItemAcc2, 0, DelItemList, UpdateItemList, UpdateItemLogList} ->
                                      {BagItemAcc2, true, DelItemList++DelItemAcc, UpdateItemList++UpdataItemAcc, UpdateItemLogAcc++UpdateItemLogList};
                                  _ ->
                                      {AccList, false, [],[],[]}
                              end;
                           false ->
                               {AccList,false, [],[],[]}
                       end
                end, {Items,true, [],[],[]}, NeedItemList),
    case erlang:element(2, ResultInfo) of
        false ->
            {false, Code};
        _ ->
            ResultInfo
    end.

%% 精灵消耗
check_ger(NeedGerList, GerID) ->
    GerBag = role_data:get_gerBag(),
    case lists:keytake(GerID, #gerSimple.gerID, GerBag) of
        false ->
            {false, 8};
        {value, DelGer=#gerSimple{gerTypeID=GerTypeID,gerQuality=Rank}, GerBag2} ->
            case NeedGerList of
                [] ->
                    {true, GerBag2, DelGer};
                [#new_ger{gerTypeID=NGerTypeID, gerQuality=NRank}|_] ->
                    case GerTypeID =:= NGerTypeID of
                        false ->
                            {false, 9};
                        _ ->
                            case Rank >= NRank of
                                true ->
                                    {true, GerBag2, DelGer}
                            end
                    end
            end
    end.

%% 消耗判断
check_cost(Role, GerID, Cost) ->
    #sell_reward{coin=Coin, gold=Gold, item=NeedItemList, reputation=Reputation, newGer=NeedGerList} = Cost,
    MoneyCostT = [{coin, Coin}, {gold, Gold}, {reputation, Reputation}],
    MoneyCost = lists:filter(fun({_,Money}) -> Money =/= 0 end, MoneyCostT),
    case check_money(Role, MoneyCost, [4,5,6]) of
        {false, Code} ->
            {false, Code};
        _ ->
            case check_item(NeedItemList, 7) of
                {false, Code} ->
                    {false, Code};
                ItemCost ->
                    case check_ger(NeedGerList, GerID) of
                        {false, Code} ->
                            {false, Code};
                        GerCost ->
                            do_cost(Role, MoneyCost, ItemCost, GerCost)
                    end
            end
    end.

%% 扣除消耗
do_cost(Role, MoneyCost, ItemCost, GerCost) ->
    #role{roleID=RoleID} = Role,
    {Date, _} = Time = erlang:localtime(),  
    % 扣货币
    case MoneyCost of
        [] ->
            ignore;
        _ ->
            lists:foldl(fun({Type, Gold}, RoleAcc) -> 
                            role_lib:deduct_money_f(RoleAcc, Type, Gold, ?MONEY_DEC_TYPE_MAGICBOOK_SWALLOW, 0, "") 
                        end, Role, MoneyCost) 
    end,
    % 扣物品
    case ItemCost of
        void ->
            ignore;
        _ ->
            {ItemList2,_,DelItemList2,UpdateItemList2,UpdateItemLogList2} = ItemCost,  
            role_data:set_bagItem(ItemList2),

            %% 物品更新
            case UpdateItemList2 of
                [] ->
                    ignore;
                _ ->
                    UpdateItemList3 = [#p_item_num_update{itemUID=ItemUID,itemNum=ItemNum}||#item{itemUID=ItemUID,itemNum=ItemNum}<-UpdateItemList2],
                    ?sendself(#sc_item_update{updateList=UpdateItemList3})
            end,

            %% 物品删除
            case DelItemList2 of 
                [] ->
                    ignore;
                _ ->
                    DelItemList3 = [E||#item{itemUID=E}<-DelItemList2],
                    ?sendself(#sc_item_delete_notify{itemUIDList=DelItemList3})
            end,                    
            
            %% 写道具日志
            LogItemList = role_item:itemList2logItemList(DelItemList2, UpdateItemLogList2),
            behavior_item_consume:log(RoleID, LogItemList, Date, Time, ?MONEY_DEC_TYPE_MAGICBOOK_SWALLOW, 0, "") 
    end,
    % 扣精灵
    {_, GerBag2, #gerSimple{gerID=GerID, gerTypeID=GerTypeID, gerLevel=GerLevel, gerQuality=GerQuality}} = GerCost,
    LogGerList= [[GerID,GerTypeID,GerLevel,GerQuality]],
    erlang:send(self(), {ger_del_cancel_cirrus, GerID}),
    behavior_ger_consume:log(RoleID, LogGerList, Date, Time, ?MONEY_DEC_TYPE_MAGICBOOK_SWALLOW, 0, ""),
    role_data:set_gerBag(GerBag2),
    DelGerIDList = [GerID],
    ?sendself(#sc_ger_del{gerIDList=DelGerIDList}).

% 判断对应魔典是否可以进行吞噬操作
check_book_can_op(BookStr, BookID, Op, CodeList) ->
    MaxOpenBookID = erlang:length(BookStr),
    case BookID > MaxOpenBookID of
        true ->
            {false, lists:nth(1, CodeList)};
        _ ->
            BookState = lists:nth(BookID, BookStr),
            Code2 = lists:nth(2, CodeList),
            case Op of
                swallow ->
                    %% 判断前一本是否解锁了
                    PrevState = 
                        case BookID > 1 of
                            false ->
                                $9;
                            _ ->
                                PrevID = BookID - 1,
                                lists:nth(PrevID, BookStr) 
                        end,

                    case PrevState >= $3 of
                        false ->
                            {false, Code2};
                        _ ->
                            case BookState >= $3 of
                                true ->
                                    {false, lists:nth(3, CodeList)};
                                _ ->
                                    true
                            end 
                    end;
                book_reward ->
                    case BookState =/= $3 of
                        true ->
                            {false, Code2};
                        _ ->
                            true 
                    end;
                picture_reward ->
                    case BookState =< $1 of
                        true ->
                            {false, Code2};
                        _ ->
                            true
                    end 
            end
    end.

%% 判断对应的图鉴是否能进行吞噬操作
check_picture_can_swallow(PictureList, Order, Pos, PictureStr) ->
    PictureID = util:safe_nth(Order, PictureList),
    case PictureID =:= null of
        true ->
            {false, 2};
        _ ->
            case Pos > ?MAX_MAGIC_PICTURE_POS of
                true ->
                    {false, 3};
                _ ->
                    PictureStateT = lists:nth(Order, PictureStr),
                    PictureState = PictureStateT - $0, 
                    case PictureState >= 3 of
                        true ->
                            {false, 13};
                        _ ->
                            case PictureState =/= Pos - 1 of 
                                true ->
                                    {false, 14};
                                _ ->
                                    {true, PictureID}
                            end
                    end
            end
    end.
            
%% 吞噬精灵
swallow_ger(BookID, Order, GerID, Pos) ->
    #magicBook_state{summary=BookStr, picture_state=PictureListState, percent=PercentList} = role_data:get_magicBook_state(),
    Role = role_data:get_roleInfo(),
    case check_open(Role) of
        false ->
            {false, 1};
        _ ->
            #magic_book{picture_list=List} = data_magic_book:get({book, BookID}),
            case check_book_can_op(BookStr, BookID, swallow, [11,15,12]) of
                {false, Code} ->
                    {false, Code};
            _ ->
                PictureStr = lists:nth(BookID, PictureListState),
                case check_picture_can_swallow(List, Order, Pos, PictureStr) of
                    {false, Code} ->
                        {false, Code};
                    {true, PictureID} ->
                        #magic_picture{posInfo=PosInfo} = data_magic_picture:get(PictureID),
                        case lists:keyfind(Pos, #magic_gerPos.pos, PosInfo) of
                            false ->
                                {false, 3};
                            #magic_gerPos{cost=Cost, buffer=Buffer} ->
                                case check_cost(Role, GerID, Cost) of
                                    {false, Code} ->
                                        {false, Code};
                                    _ ->
                                        %% 加上加成,重新计算属性
                                        AddAttr = role_data:get_magicBook_add_attr(),
                                        NewAddAttr = ger_attr:append_add_attr(AddAttr, Buffer),
                                        role_data:set_magicBook_add_attr(NewAddAttr),
                                        ger_attr:recacl_gers(),
                                        
                                        %% 修改图鉴状态
                                        PictureStr2 = util:nth_replace(Order, PictureStr, Pos + $0),
                                        PictureListState2 = util:nth_replace(BookID, PictureListState, PictureStr2),

                                        %% 判断魔典是否收集满了
                                        CurState = 
                                            case Pos =:= ?MAX_MAGIC_PICTURE_POS andalso lists:all(fun(E) -> E >= $3 end, PictureStr2) of
                                                true ->
                                                    $3;
                                                _ ->
                                                    $2
                                            end,

                                        BookStr2 = util:nth_replace(BookID, BookStr, CurState),

                                        %% 修改完成百分比
                                        {Has, Total} = lists:nth(BookID, PercentList), 
                                        PercentList2 = util:nth_replace(BookID, PercentList, {Has + 1, Total}),

                                        %% 判断是否解锁下一本
                                        {BookStr3, PictureListState3, PercentList3} = 
                                            unlock_next(BookID, Role, CurState, BookStr2, PictureListState2, PercentList2),
    
                                        role_data:set_magicBook_state({magicBook_state, BookStr3, PictureListState3, PercentList3}),
										{Date, _} = Time = erlang:localtime(),  
										behavior_magicBook_update:log(role_data:get_roleID(),Date,Time,BookID,PictureStr2),
                                        {true, 0}
                                end
                        end
                end
            end
    end.

% 生成魔典简要信息
gen_magicBook_summary(?MAX_MAGIC_BOOK_NUM + 1, _, _, Summary) ->
    lists:reverse(Summary);
gen_magicBook_summary(BookID, [], [], Summary) ->
    #magic_book{open_level=OpenLevel} = data_magic_book:get({book, BookID}),
    gen_magicBook_summary(BookID + 1, [], [], [#p_magicBook_summary{level=OpenLevel, state=0, percent=0}|Summary]);
gen_magicBook_summary(BookID, [H|T], [{Has,Total}|PT], Summary) ->
    #magic_book{open_level=OpenLevel} = data_magic_book:get({book, BookID}),
    gen_magicBook_summary(BookID + 1, T, PT, [#p_magicBook_summary{level=OpenLevel, state=H - $0, percent=erlang:trunc(Has/Total * 100)}|Summary]).

% 领取图鉴奖励
get_picture_reward(BookID, Order) ->
    Role = role_data:get_roleInfo(),
    case check_open(Role) of
        false ->
            {false, 1};
        _ ->
            #magicBook_state{summary=BookStr, picture_state=PictureState} = BookState =  role_data:get_magicBook_state(),
            case check_book_can_op(BookStr, BookID, picture_reward, [2,3]) of
                {false, Code} ->
                    {false, Code};
                _ ->
                    PictureStr = lists:nth(BookID, PictureState),
                    #magic_book{picture_list=PictureList} = data_magic_book:get({book, BookID}),
                    PictureID = util:safe_nth(Order, PictureList),
                    case PictureID =:= null of
                        true ->
                            {false, 4};
                        _ ->
                            CPictureState = lists:nth(Order, PictureStr),
                            case CPictureState =:= $9 of
                                true ->
                                    {false, 5};
                                _ ->
                                    case CPictureState =/= $3 of
                                        true ->
                                            {false, 6};
                                        _ ->
                                            #magic_picture{reward=Reward} = data_magic_picture:get(PictureID),
                                            role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_MAGICBOOK_PICTURE_REWARD, 0, ""),

                                            % 修改图鉴状态
                                            PictureStr2 = util:nth_replace(Order, PictureStr, $9),
                                            PictureListState2 = util:nth_replace(BookID, PictureState, PictureStr2),
                                            role_data:set_magicBook_state(BookState#magicBook_state{picture_state=PictureListState2}),
                                            {true, 0}
                                    end
                            end
                    end
            end
    end.

% 领魔典奖励
get_book_reward(BookID) ->
    Role = role_data:get_roleInfo(),
    case check_open(Role) of
        false ->
            {false, 1};
        _ ->
            #magicBook_state{summary=BookStr} = BookState = role_data:get_magicBook_state(),
            case check_book_can_op(BookStr, BookID, book_reward, [2,3]) of
                {false, Code} ->
                    {false, Code};
                _ ->
                    #magic_book{reward=Reward} = data_magic_book:get({book, BookID}),
                    role_reward:handle_sell_reward_f(Role, Reward, ?MONEY_ADD_TYPE_MAGICBOOK_PICTURE_REWARD, 0, ""),

                    % 修改图鉴状态
                    BookStr2 = util:nth_replace(BookID, BookStr, $9),
                    role_data:set_magicBook_state(BookState#magicBook_state{summary=BookStr2}),
                    {true, 0}
            end
    end.


%% 解锁下一本
unlock_next(BookID, #role{level=Level}, CurState, BookStr, PictureState, PercentList) ->
    case CurState of
        $2 ->
            {BookStr, PictureState, PercentList};
        _ ->
            case BookID >= ?MAX_MAGIC_BOOK_NUM of
                true ->
                    {BookStr, PictureState, PercentList};
                _ ->
                    NewBookID = BookID + 1,
                    #magic_book{open_level=OpenLevel, picture_list=PictureList} = data_magic_book:get({book, NewBookID}),
                    case OpenLevel > Level of
                        true ->
                            {BookStr, PictureState, PercentList};
                        _ ->
                            BookStr2 = BookStr ++ "1",
                            PictureStr = gen_picture_state_str(PictureList),
                            PictureState2 = PictureState ++ [PictureStr],
                            Total = erlang:length(PictureStr) * ?MAX_MAGIC_PICTURE_POS,
                            PercentList2 = PercentList ++ [{0, Total}],
                            {BookStr2, PictureState2, PercentList2}
                    end
            end 
    end.

% 升级时判断是否解锁下个魔典
hook_level_up(NewLevel) ->
    #magicBook_state{summary=BookStr, picture_state=PictureState, percent = PercentList} = role_data:get_magicBook_state(),
    MaxBookID = erlang:length(BookStr),
    case MaxBookID >= ?MAX_MAGIC_BOOK_NUM of
        true ->
            ignore;
        _ ->
            %% 还需要判断是否收集满了当前这本
            CurState = 
                case MaxBookID =:= 0 of 
                    true ->
                        $9;
                    _ ->
                        lists:nth(MaxBookID, BookStr) 
                end,
            case CurState >= $3 of
                false ->
                    ignore;
                _ ->
                    NewBookID = MaxBookID + 1,
                    #magic_book{open_level=OpenLevel, picture_list=PictureList} = data_magic_book:get({book, NewBookID}),
                    case OpenLevel > NewLevel of
                        true ->
                            ignore;
                        _ ->
                            BookStr2 = BookStr ++ "1",
                            PictureStr = gen_picture_state_str(PictureList),
                            PictureState2 = PictureState ++ [PictureStr],
                            Total = erlang:length(PictureStr) * ?MAX_MAGIC_PICTURE_POS,
                            PercentList2 = PercentList ++ [{0, Total}],
                            role_data:set_magicBook_state({magicBook_state, BookStr2, PictureState2, PercentList2})
                    end
            end
    end.

% 生成图鉴状态列表字符串
gen_picture_state_str([], StateStr) ->
    StateStr;
gen_picture_state_str([_|T], StateStr) ->
    gen_picture_state_str(T, StateStr ++ "0").

gen_picture_state_str(PictureList) ->
    gen_picture_state_str(PictureList, "").

% 发送魔典详细信息
send_magicBook_book_detial(BookID) ->
    #magicBook_state{summary=Summary, picture_state=PictureState, percent=PercentList} = role_data:get_magicBook_state(),
    case util:safe_nth(BookID,Summary) of
        null ->
            ignore;
        StateStr ->
            PictureStr = lists:nth(BookID, PictureState),
            BookState = lists:nth(BookID, Summary),
            {Has,Total} = lists:nth(BookID, PercentList),
            Buffer = calc_magicBook_add_attr(BookID, BookState, PictureStr),
            ?sendself(#sc_magicBook_book_detial{attr=Buffer, picture=PictureStr, state=StateStr - $0, percent=erlang:trunc(Has/Total * 100)}) 
    end.

% 获得某本书当前的加成和
calc_magicBook_add_attr(BookID, BookState, PictureStr) ->
    case BookState of
        $0 ->
            erlang:setelement(1, transform_addattr2old_addattr(#add_attr{}), p_magicBook_attr);
        _ ->
            BufferT1 = role_data:calc_magicBook_add_attr([BookState], [PictureStr], BookID, #add_attr{}),
            BufferT = transform_addattr2old_addattr(BufferT1),
            erlang:setelement(1, BufferT, p_magicBook_attr) 
    end.

transform_addattr2old_addattr(AddAttr) when is_record(AddAttr,add_attr)->
    List1 = tuple_to_list(AddAttr),
    List2 = lists:sublist(List1,length(List1)-6),
    list_to_tuple(List2);
transform_addattr2old_addattr(AddAttr) ->
    ?ERR("出现非add_attr结构转换：~w ~n",[AddAttr]),
    transform_addattr2old_addattr(#add_attr{}).