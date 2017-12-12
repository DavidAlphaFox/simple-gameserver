-module(test_proto).

-compile(export_all).

-record(sc_fight_begin,{
	target
	,name
	,joiners
	,helpers
}).
-record(cs_name,{
	a
}).

-define(undef, undefined).

%% proto #sc_fight_begin{target=1,name="sdfsdfe",joiners=[1,2,3,4,5],helpers=[#cs_name{a=23},#cs_name{a=42}]}

encode_def(sc_fight_begin)->
	[1,int64,string,{list,int32},{list,tuple}];
encode_def(cs_name) ->
	[2,int8];
encode_def(_) ->
	?undef.

decode_def(1) ->
	[sc_fight_begin,int64,string,{list,int32},{list,tuple}];
decode_def(2) ->
	[cs_name,int8];
decode_def(_) ->
	?undef.


for(N,N,_F)->
	finish;
for(M,N,F) ->
	F(),
	for(M+1,N,F).

now_sec() ->
	{A,B,C}=erlang:now(),
	A*1000000 + B + C/1000000.

man1(N) ->
	Begin = now_sec(),
	for(1,N, fun() -> encode_record(#sc_fight_begin{target=1,name="sdfsdfe",joiners=[1,2,3,4,5],helpers=[#cs_name{a=23},#cs_name{a=42}]}
) end),
	End = now_sec(),
	io:format("manual encode consume ~wus per times \n",[1000000*(End - Begin)/N]).

man2(N) ->
	Begin = now_sec(),
	for(1,N, fun() -> decode(<<0,0,1,0,0,0,0,0,0,0,1,0,7,115,100,102,115,100,102,101,0,
  5,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,4,0,0,0,5,0,2,0,2,23,0,
  2,42>>
) end),
	End = now_sec(),
	io:format("manual decode consume ~wus per times \n",[1000000*(End - Begin)/N]).

auto1(N) ->
	Begin = now_sec(),
	for(1,N, fun() -> term_to_binary(#sc_fight_begin{target=1,name="sdfsdfe",joiners=[1,2,3,4,5],helpers=[#cs_name{a=23},#cs_name{a=42}]}
) end),
	End = now_sec(),
	io:format("auto encode consume ~wus per times \n",[1000000*(End - Begin)/N]).


auto2(N) ->
	Begin = now_sec(),
	for(1,N, fun() -> binary_to_term(<<131,104,5,100,0,14,115,99,95,102,105,103,104,116,95,98,
  101,103,105,110,97,1,107,0,7,115,100,102,115,100,102,
  101,107,0,5,1,2,3,4,5,108,0,0,0,2,104,2,100,0,7,99,115,
  95,110,97,109,101,97,23,104,2,100,0,7,99,115,95,110,97,
  109,101,97,42,106>>
) end),
	End = now_sec(),
	io:format("auto decodeconsume ~wus per times \n",[1000000*(End - Begin)/N]).

encode_record(Record)->
	Bin = iolist_to_binary(encode(tuple, Record)),
	ByteSize = erlang:byte_size(Bin),
	if ByteSize > 1024 ->
		Bin2 = zlib:compress(Bin),
		ByteSize2 = erlang:byte_size(Bin2),
		<<ByteSize2:16,1:8,Bin2/binary>>;
	true ->
		<<ByteSize:16,0:8,Bin/binary>>
	end.

encode(int64, A) ->
	<<A:64>>;
encode(int32, A) ->
	<<A:32>>;
encode(int16, A) ->
	<<A:16>>;
encode(int8,  A) ->
	<<A:8>>;
encode(string, A) ->
	Bin = iolist_to_binary(A),
	BitSize=  erlang:byte_size(Bin),
	<<BitSize:16, Bin/binary>>;
encode(tuple, Record) ->
	[TupleType|VarList] = tuple_to_list(Record),
	[ID|TypeList] = encode_def(TupleType),
	Bin = lists:zipwith(fun encode/2, TypeList, VarList),
	[<<ID:16>>, Bin];
encode(bool, A) ->
	if A=:=true ->
		<<1:8>>;
	true ->
		<<0:8>>
	end;
encode({list, Type}, A) ->
	Len = length(A),
	Bin = [encode(Type, Element)||Element<-A],
	[<<Len:16>>, Bin].

decode(<<0:8,Bin/binary>>) ->
	decode2(Bin);
decode(<<1:8,Bin/binary>>) ->
	decode2(zlib:uncompress(Bin)).

decode2(<<ID:16,Bin/binary>>) ->
	[RecName|TypeList] = decode_def(ID),
	{ok, _, Result} = decode3(TypeList, Bin, [RecName]),
	list_to_tuple(Result).


decode3([int64|TypeList],Bin,Result) ->
	<<Int:64, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([int32|TypeList],Bin,Result) ->
	<<Int:32, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([int8|TypeList],Bin,Result) ->
	<<Int:8, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [Int|Result]);
decode3([string|TypeList],Bin,Result) ->
	<<Len:16, StrBin:Len/binary-unit:8, Bin2/binary>> = Bin,
	decode3(TypeList, Bin2, [binary_to_list(StrBin)|Result]);
decode3([bool|TypeList], Bin, Result) ->
	<<Bool:8, Bin2/binary>> = Bin,
	BoolErl = 
	if Bool =:= 1 ->
		true;
	true ->
		false
	end,
	decode3(TypeList, Bin2, [BoolErl|Result]);
decode3([{list,Type}|TypeList],Bin, Result) ->
	<<Len:16, Bin2/binary>> = Bin,
	ChildTypeList = lists:duplicate(Len, Type),
	{ok, Bin3, List} = decode3(ChildTypeList, Bin2, []),
	decode3(TypeList, Bin3, [List|Result]);
decode3([tuple|TypeList], Bin, Result) ->
	<<ID:16, Bin2/binary>> = Bin,
	[RecName|ChildTypeList] = decode_def(ID),
	{ok, Bin3, ChildVal} = decode3(ChildTypeList, Bin2, [RecName]),
	decode3(TypeList, Bin3, [list_to_tuple(ChildVal)|Result]);
decode3([],Bin,Result) ->
	{ok, Bin, lists:reverse(Result)}.



