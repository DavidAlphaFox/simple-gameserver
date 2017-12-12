-module(doublematch_sup).
-behaviour(supervisor).
-export([init/1,start_link/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildList = case data_setting:get(server_type) of
        carlos_match ->
            DoublematchRank = 
                {doublematch_rank
                ,{doublematch_rank, start_link, []}
                ,permanent, 600000, worker, [doublematch_rank]},
            DoublematchMatch = 
                {doublematch_match
                ,{doublematch_match, start_link, []}
                ,permanent, 600000, worker, [doublematch_match]},
            [DoublematchRank,DoublematchMatch];
        _ ->
            []
    end,
    {ok,{{one_for_one,3,10}, ChildList}}.
