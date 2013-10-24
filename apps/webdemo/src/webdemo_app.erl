-module(webdemo_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         is_wm_tracing/0,
         set_wm_tracing/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = webdemo_sup:start_link(),
    ok = wmtrace_resource:add_dispatch_rule("wmtrace", "/tmp"),
    ok = start_listeners(),
    {ok, Pid}.

start_listeners() ->
    start_ws_listener(),
    ok.

start_ws_listener() ->
    Routes = [{'_', [{"/[...]", ws_handler, []}]}],
    Dispatch = cowboy_router:compile(Routes),
    Ref = my_ws_listener,
    NbAcceptors = 100,
    TransOpts = [{port, 8001}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _Pid2} = cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts).

stop(_State) ->
    ok.

is_wm_tracing() ->
    application:get_env(webdemo, wm_tracing, false).

set_wm_tracing(IsTracing) ->
    ok = application:set_env(webdemo, wm_tracing, IsTracing).
