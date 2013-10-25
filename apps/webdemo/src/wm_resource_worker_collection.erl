-module(wm_resource_worker_collection).

-export([init/1,
         allowed_methods/2,
         post_is_create/2,
         create_path/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_whatever/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(_Config) ->
    Context = [],
    Result = case webdemo_app:is_wm_tracing() of
                 true -> {trace, "/tmp"};
                 false -> ok
             end,
    {Result, Context}.

content_types_provided(ReqData, Context) ->
    Result = [{"application/json", to_json}],
    {Result, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    ContentType = wrq:get_req_header("content-type", ReqData),
    {[{ContentType, from_whatever}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    Result = ['GET', 'POST'],
    {Result, ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {ok, Id} = workers:start(),
    {Id, ReqData, Context}.

from_whatever(ReqData, Context) ->
    {true, ReqData, Context}.

to_json(ReqData, Context) ->
    Response = case wrq:method(ReqData) of
                   'POST' -> "";
                   'GET' -> workers_to_json(workers:list())
               end,
    {Response, ReqData, Context}.

workers_to_json(WorkerList) ->
    IdArrayTerm = lists:map(fun({Id, _Pid}) ->
                                    list_to_binary(Id)
                            end, WorkerList),
    mochijson2:encode(IdArrayTerm).
