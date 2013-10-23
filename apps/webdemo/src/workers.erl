%%%-------------------------------------------------------------------
%%% @doc Interface to the workers
%%%
%%% Users shouldn't need to know how they're managed or where they are.
%%% @end
%%%-------------------------------------------------------------------

-module(workers).

-export([start/0,
         stop/1,
         list/0
        ]).

start() ->
    Id = uuid:to_string(uuid:uuid4()),
    {ok, _Pid} = worker_sup:start_worker(Id),
    {ok, Id}.

stop(Id) ->
    worker_sup:stop_worker(Id).

list() ->
    worker_sup:list_workers().
