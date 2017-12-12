%%%-------------------------------------------------------------------
%%% @author admin <>
%%% @copyright (C) 2012, admin
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(merge_log).

%% API
-export([
         start_log_server/0
        ]).

-define(LOG_FILE, do_merge:get(merge_log_path)).

%%%===================================================================
%%% API
%%%===================================================================

start_log_server( ) ->
    Pid = erlang:spawn(fun() -> loop_log_server() end),
    erlang:put(merge_log_server, Pid),
    ok.

sendto_log_server(Msg) ->
    Pid = erlang:get(merge_log_server),
    Pid ! Msg.

loop_log_server() ->
    receive 
        {log, Module, Line, Time, Format, Args} ->
            do_write_log(Module, Line, Time, Format, Args);
        R ->
            M = io_lib:format("~ts:~p", ["收到未知消息", R]),
            file:write_file(?LOG_FILE, M, [append, delayed_write]),
            ok
    end,
    loop_log_server().

do_write_log(Module, Line, Time, Format, Args) ->
    {{Y,Mo,D},{H,Mi,S}} = Time,
    Time2 = io_lib:format("==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w === ~10s:~.5.0w",
                          [Y, Mo, D, H, Mi, S, Module, Line]),
    L2 = lists:concat(["merge_log ", Time2]),
    B = unicode:characters_to_binary(L2),
    file:write_file(?LOG_FILE, B, [append, delayed_write]),
    try 
        M = io_lib:format(Format, Args),
        file:write_file(?LOG_FILE, M, [append, delayed_write])
    catch _:Error ->
            io:format("log error ~p ~p ~p", [Error, Format, Args])
    end,
    ok.
