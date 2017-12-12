%% @author caohongyang
%% @doc 配置管理进程
%% Created 2013-2-19


-module(tk_config).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
-include("data.hrl").
-include("preload_config.hrl").
-export([start_link/0,preload_config/0,root_dir/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).


-record(state, {}).

%% @doc 获取服务器根目录
root_dir() ->
	filename:dirname(filename:dirname(element(2,code:is_loaded(?MODULE)))).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc gen_server:init/1
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% 此进程目前只做了初始化的事情，初始化完成后直接hibernate
init([]) ->
	preload_config(),
	random2:init(),
	erlang:garbage_collect(),
    {ok, #state{}, hibernate}.


%% @doc gen_server:handle_call/3
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
handle_call(Request, _From, State) ->
	?ERR("handle_call function clause:request=~100p",[Request]),
    Reply = ok,
    {reply, Reply, State}.


%% @doc gen_server:handle_cast/2
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_cast(Msg, State) ->
	?ERR("handle_cast function clause:request=~100p",[Msg]),
    {noreply, State}.


%% @doc gen_server:handle_info/2
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_info(Info, State) ->
	?ERR("handle_info function clause:request=~100p",[Info]),
    {noreply, State}.

%% @doc gen_server:terminate/2
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(Reason, State) ->
	?ERR("~w terminate for \nReason=~300p\nState=~300p\nDictionary=~10000p",[Reason,  State, element(2,process_info(self(),dictionary))]), 
    ok.


%% @doc gen_server:code_change/3
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 预加载配置
preload_config() ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	lists:foreach(fun({ConfigPath, Module, Type, TransformFun}) ->
							io:format("load config ~s\n",[ConfigPath]),
						  config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun);
					 (Fun) when is_function(Fun,1) ->
						  Fun(OutDir);
					 (Fun) when is_function(Fun,0) ->
						  Fun()					 
				  end, ?PRELOAD_CONFIG).

reload_config(ConfigMod) ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	case lists:keyfind(ConfigMod, 2, ?PRELOAD_CONFIG) of
		false ->
			io:format("young boy ,this config is new，please try reload like this: tk_config:reload_config(\"config/data_skill.config\",data_skill,key_value,original).");
		{ConfigPath, Module, Type, TransformFun} ->
			config2erl:beam(filename:join([RootDir, ConfigPath]), Module, OutDir, Type, TransformFun),
			c:l(Module)
	end.

reload_config(ConfigRelativePath, ModuleAtom, Type, TransformFun) ->
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	config2erl:beam(filename:join([RootDir, ConfigRelativePath]), ModuleAtom, OutDir, Type, TransformFun),
	c:l(ModuleAtom).
	

%% 加载宝物碎片配置
load_data_patch() ->
	io:format("load_data_patch\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	{KeyValList1,KeyValList2} = 
	lists:foldl(fun(ComposeID, {Acc1,Acc2}) ->
						#data_compose{product = ProductInfo,needItemList=NeedList}= data_compose:get(ComposeID),
                        ProductInfoID = ProductInfo#new_item.itemTypeID,
%% 						L1 = [{TypeID,ProductInfoID}||{TypeID,_} <-NeedList],
%%                         L2 = [{ProductInfoID,TypeID}||{TypeID,_} <-NeedList],
                        [{TypeID1,_},{TypeID2,_},{TypeID3,_},{TypeID4,_}] = NeedList,
                        L1 = [{TypeID1,{ProductInfoID,1}},{TypeID2,{ProductInfoID,2}},{TypeID3,{ProductInfoID,3}},{TypeID4,{ProductInfoID,4}}],
                        L2 = [{{ProductInfoID,1},TypeID1},{{ProductInfoID,2},TypeID2},{{ProductInfoID,3},TypeID3},{{ProductInfoID,4},TypeID4}],
						{L1++Acc1,L2++Acc2}
				end, {[],[]}, data_compose:get_list()),
	config2erl:beam2(KeyValList1, data_patch_parts, OutDir, key_value, orignal),
    config2erl:beam2(KeyValList2, data_patch_product, OutDir, key_value, orignal).

%% 加载主公等级-经验，映射配置
load_data_role_level() ->
	io:format("load data_role_level\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	KeyValList = [{data_role_exp:get(E),E} || E<-data_role_exp:get_list()],
	config2erl:beam2(KeyValList, data_role_level, OutDir, key_value, orignal).
					
%% 加载武将等级-经验，映射配置
load_data_ger_level() ->
	io:format("load data_ger_level\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	KeyValList = [{data_ger_exp:get(E),E} || E<-data_ger_exp:get_list()],
	config2erl:beam2(KeyValList, data_ger_level, OutDir, key_value, orignal).
load_data_ger_light_level() ->
    io:format("load data_ger_light_level\n"),
    RootDir = root_dir(),
    OutDir = filename:join([RootDir, "ebin"]),
    KeyValList = [{data_ger_exp_light:get(E),E} || E<-data_ger_exp_light:get_list()],
    config2erl:beam2(KeyValList, data_ger_level_light, OutDir, key_value, orignal).

%% 加载宝物品阶-经验，映射配置
load_data_treasure_rank() ->
	io:format("load data_treasure_rank\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	KeyValList = [{data_treasure_exp:get(E),E} || E<-data_treasure_exp:get_list()],
	config2erl:beam2(KeyValList, data_treasure_rank, OutDir, key_value, orignal).
load_data_treasure_rank2() ->
	io:format("load data_treasure_rank\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	KeyValList = [{data_treasure_exp2:get(E),E} || E<-data_treasure_exp2:get_list()],
	config2erl:beam2(KeyValList, data_treasure_rank2, OutDir, key_value, orignal).

%% 加载武将天命的受影响武将列表
load_data_destiny_rela() ->
	io:format("load data_destiny_rela\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	KeyValList = [begin
					  #data_ger{destinyIDList=DesIDList}=data_ger:get(GerTypeID),
					  RelaGerList = lists:foldl(fun(DesID, Acc) ->
														#data_destiny{destinyType=T, destinyNeedList=NeedList} = data_destiny:get(DesID),
														if T =:= 1 ->
															   NeedList++Acc;
														   true ->
															   Acc
														end
												end, [], DesIDList),
					  {GerTypeID, RelaGerList}
				  end || GerTypeID <- data_ger:get_list()],
	config2erl:beam2(KeyValList, data_destiny_rela, OutDir, key_value, orignal).
	
gen_seq_list(List) ->
	gen_seq_list(List,1,[]).

gen_seq_list([H|T], Seq,Result) ->
	gen_seq_list(T,Seq+1,[{Seq,H}|Result]);
gen_seq_list([],N,Result) ->
	lists:reverse([{max,N-1}|Result]).

%% 加载随机名字配置
load_data_name() ->
	io:format("load data_name\n"),
	RootDir = root_dir(),
	OutDir = filename:join([RootDir, "ebin"]),
	case file:consult(filename:join([root_dir(),"config/moduleconfig/data_name.config"])) of
		{ok, TermList} ->
			[MaleFamilyName,FemaleFamilyName,MaleGivenName,FemaleGivenName|_] = TermList,
			config2erl:beam2(gen_seq_list(MaleFamilyName), data_male_family_name, OutDir, key_value, orignal),
			config2erl:beam2(gen_seq_list(FemaleFamilyName), data_female_family_name, OutDir, key_value, orignal),
			config2erl:beam2(gen_seq_list(MaleGivenName), data_male_given_name, OutDir, key_value, orignal),
			config2erl:beam2(gen_seq_list(FemaleGivenName), data_female_given_name, OutDir, key_value, orignal);
		_ ->
			ok
	end.
			