%%%-------------------------------------------------------------------
%%% @doc Supervisor for our fake little workers
%%% @end
%%%-------------------------------------------------------------------
-module(worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_worker/1,
         stop_worker/1,
         list_workers/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_worker(any()) -> {ok, pid()}.
start_worker(Id) ->
    StartFunc = {worker, start_link, [Id]},
    Restart = transient,         % restart only when stopped by error
    Shutdown = 5,                % milliseconds
    Type = worker,
    Modules = [worker],
    ChildSpec = {Id, StartFunc, Restart, Shutdown, Type, Modules},
    {ok, _Pid} = supervisor:start_child(?SERVER, ChildSpec).

-spec stop_worker(any()) -> ok.
stop_worker(Id) ->
    ok = supervisor:terminate_child(?SERVER, Id),
    ok = supervisor:delete_child(?SERVER, Id).

-spec list_workers() -> [{any(), pid()}].
list_workers() ->
    Children = supervisor:which_children(?SERVER),
    ChildToWorkerFun = fun({Id, Child, _Type, _Modules}) ->
                               {Id, Child}
                       end,
    lists:map(ChildToWorkerFun, Children).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
