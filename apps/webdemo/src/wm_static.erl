-module(wm_static).

-export([init/1,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2,
         to_raw/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    SiteRoot = proplists:get_value(site_root, Config),
    Context = [{site_root, SiteRoot}],
    Result = case webdemo_app:is_wm_tracing() of
                 true -> {trace, "/tmp"};
                 false -> ok
             end,
    {Result, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    SiteRoot = proplists:get_value(site_root, Context),
    Path = wrq:disp_path(ReqData),
    FullPath = filename:join(SiteRoot, Path),
    CheapCanonical = re:replace(FullPath, "(/\\.\\./)", "/", [{return, list}]),
    case file:read_file(CheapCanonical) of
        {ok, Binary} ->
            NewContext = [ {file_raw, Binary},
                           {fs_path, CheapCanonical}
                         | Context],
            {true, ReqData, NewContext};
        {error, enoent} ->
            {false, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    MIMEs =  [{".css",  "text/css"},
              {".js",   "application/javascript"},
              {".html", "text/html"}
             ],
    Path = wrq:disp_path(ReqData),
    Extension = filename:extension(Path),
    Supported = case lists:keyfind(Extension, 1, MIMEs) of
                    {Extension, MIME} -> [{MIME, to_raw}];
                    false -> []
                end,
    {Supported, ReqData, Context}.

to_raw(ReqData, Context) ->
    {proplists:get_value(file_raw, Context), ReqData, Context}.
