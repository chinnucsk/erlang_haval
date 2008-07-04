%%% --------------------------------------------------------------------
%%% @doc  Application master for HAVAL
%%% @end
%%% --------------------------------------------------------------------
-module(haval_app).
-behaviour(application).

%% callbacks
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
  haval_sup:start_link().

stop(_State) ->
  ok.
