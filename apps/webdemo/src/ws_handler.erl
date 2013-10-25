-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ok = pusher:register(self()),
    {ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({Namespace, {EventType, Data}}, Req, State) ->
    JSONTerm = [{"namespace", Namespace},
                {"event", [{"type", EventType},
                           {"data", list_to_binary(Data)}
                          ]}
               ],
    JSON = mochijson2:encode(JSONTerm),
    {reply, {text, JSON}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok = pusher:deregister(self()).
