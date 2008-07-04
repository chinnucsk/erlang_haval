-module(haval_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Constnt
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% init([]) -> {ok, {SupFlags, [ChildSpec]}}
init([]) ->
  Child = {haval_server, {haval_server, start_link, []},
            permanent, 2000, worker, [haval_server]},
  {ok, {{one_for_all, 10, 3600}, [Child]}}.
