%%%-------------------------------------------------------------------
%%% @doc Top supervisor for our demo app
%%% @end
%%%-------------------------------------------------------------------
-module(webdemo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChildSpecs = [?CHILD(pusher, worker),
                  ?CHILD(worker_sup, supervisor),
                  webmachine_instance_childspec()
                 ],

    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

webmachine_instance_childspec() ->
    SiteRoot = filename:join(code:priv_dir(webdemo), "docroot"),
    Dispatch = [{["worker", id], wm_resource_worker, []},
                {["worker"], wm_resource_worker_collection, []},
                {['*'], wm_static, [{site_root, SiteRoot}]}
               ],
    Config = [{ip, "0.0.0.0"},
              {port, 8003},
              {dispatch, Dispatch}
              ],
    {wm_instance, {webmachine_mochiweb, start, [Config]},
     permanent, 5000, worker, dynamic}.
