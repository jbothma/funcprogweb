-module(webdemo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes = [{'_',
               [{"/[...]", cowboy_static,
                 [{directory, {priv_dir, webdemo, [<<"docroot">>]}}
                 ]}
               ]}],
    Dispatch = cowboy_router:compile(Routes),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(my_http_listener, 100,
                      [{port, 8002}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    webdemo_sup:start_link().

stop(_State) ->
    ok.
