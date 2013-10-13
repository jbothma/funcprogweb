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
    StaticRoutes =
        [{'_',
          [{"/[...]", cowboy_static,
            [{directory, {priv_dir, webdemo, [<<"docroot">>]}},
             {mimetypes,
              [{<<".css">>, [<<"text/css">>]},
               {<<".js">>, [<<"application/javascript">>]},
               {<<".html">>, [<<"text/html">>]}
              ]}
            ]}
          ]}],
    StaticDispatch = cowboy_router:compile(StaticRoutes),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _Pid} = cowboy:start_http(my_http_listener, 100,
                                  [{port, 8002}],
                                  [{env, [{dispatch, StaticDispatch}]}]
                                 ),
    WSRoutes = [{'_', [{"/[...]", ws_handler, []}]}],
    WSDispatch = cowboy_router:compile(WSRoutes),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, _Pid2} = cowboy:start_http(my_ws_listener, 100,
                                  [{port, 8001}],
                                  [{env, [{dispatch, WSDispatch}]}]
                                 ),
    ok.

stop(_State) ->
    ok.
