%% @author caohongyang
%% @doc 基本的性能测试函数
%% Created 2013-2-27


-module(tc).

%% API functions
-export([run/2]).
-export([for0/3]).
-export([for1/3]).

%% Internal functions
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

run(N,F) ->
	spawn(fun() -> run2(N,F) end).
run2(N, F) when is_function(F,0) ->
	erlang:statistics(runtime),
	erlang:statistics(wall_clock),
	for0(1,N,F),
	{_, Time1} = erlang:statistics(runtime),
	{_, Time2} = erlang:statistics(wall_clock),
	U1 = Time1 * 1000 / N,
	U2 = Time2 * 1000 / N,
	io:format("apply fun time=~p (~p) microseconds~n",[U1,U2]);
run2(N, F) when is_function(F,1) ->
	erlang:statistics(runtime),
	erlang:statistics(wall_clock),
	for1(1,N,F),
	{_, Time1} = erlang:statistics(runtime),
	{_, Time2} = erlang:statistics(wall_clock),
	U1 = Time1 * 1000 / N,
	U2 = Time2 * 1000 / N,
	io:format("apply fun time=~p (~p) microseconds~n",[U1,U2]).

for0(N,N,F) ->
	F();
for0(M,N,F) ->
	F(),
	for0(M+1,N,F).

for1(N,N,F) ->
	F(N);
for1(M,N,F) ->
	F(M),
	for1(M+1,N,F).

%% ====================================================================
%% Internal functions
%% ====================================================================


