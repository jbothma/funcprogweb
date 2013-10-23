-module(wm_resource_worker).

-export([init/1,
         content_types_provided/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(_FromDispatch) ->
    Context = [],
    {ok, Context}.

content_types_provided(ReqData, Context) ->
    Result = [],
    {Result, ReqData, Context}.
