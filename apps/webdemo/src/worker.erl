-module(worker).

-export([start_link/0]).

start_link() ->
    Pid = spawn(fun sit_and_wait/0),
    {ok, Pid}.

sit_and_wait() ->
    receive
        Something ->
            Something
    end.
