-module(doublematch_app).
-behaviour(application).
-include("common.hrl").
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    doublematch_sup:start_link().

stop(_State) ->
    ok.

