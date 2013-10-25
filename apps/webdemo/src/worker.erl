-module(worker).

-export([start_link/1,
         sit_and_wait/1
        ]).

start_link(Id) ->
    Pid = spawn(?MODULE, sit_and_wait, [Id]),
    {ok, Pid}.

sit_and_wait(Id) ->
    process_flag(trap_exit, true),
    pusher:broadcast({workers, {start, Id}}),
    ExitReason = receive
                     Something ->
                         case Something of
                             {'EXIT', _FromPid, Reason} ->
                                 Reason;
                             _ ->
                                 Something
                         end
                 end,
    pusher:broadcast({workers, {stop, Id}}),
    exit(ExitReason).
