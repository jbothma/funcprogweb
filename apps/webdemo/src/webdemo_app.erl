-module(webdemo_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = webdemo_sup:start_link(),
    ok = start_listeners(),
    {ok, Pid}.

start_listeners() ->
    start_http_listener(),
    start_ws_listener(),
    ok.

start_http_listener() ->
    Host = '_',
    DirOpt = {directory, {priv_dir, webdemo, [<<"docroot">>]}},
    MIMEOpt = {mimetypes,
               [{<<".css">>, [<<"text/css">>]},
                {<<".js">>, [<<"application/javascript">>]},
                {<<".html">>, [<<"text/html">>]}
               ]},
    Opts = [DirOpt, MIMEOpt],
    Paths = [{"/[...]", cowboy_static, Opts}],
    Routes = [{Host, Paths}],
    Dispatch = cowboy_router:compile(Routes),
    Ref = my_http_listener,
    NbAcceptors = 100,
    TransOpts = [{port, 8002}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _Pid} = cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts).

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
