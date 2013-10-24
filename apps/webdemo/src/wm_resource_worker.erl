-module(wm_resource_worker).

-export([init/1,
         allowed_methods/2,
         delete_resource/2,
         delete_completed/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Config) ->
    Context = [],
    Result = case webdemo_app:is_wm_tracing() of
                 true -> {trace, "/tmp"};
                 false -> ok
             end,
    {Result, Context}.

allowed_methods(ReqData, Context) ->
    Result = ['DELETE'],
    {Result, ReqData, Context}.

delete_resource(ReqData, Context) ->
    Id = wrq:path_info(id, ReqData),
    ok = workers:stop(Id),
    {true, ReqData, Context}.

delete_completed(ReqData, Context) ->
    {true, ReqData, Context}.
