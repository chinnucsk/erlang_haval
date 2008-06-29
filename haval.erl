-module(haval).
-author('Rakuto Furutani <xri://=rakuto>').

-define(DRV_INFO, 1).
-define(DRV_HAVAL_STRING, 2).
-define(DRV_HAVAL_FILE, 3).

-export([start/0, stop/0]).
-export([info/0, haval_string/1, haval_file/1]).
-ifdef(debug).
-export([test/0, test/1]).
-endif.

%%
%% Public interface
%%
start() ->
  start("haval_drv"),
  ok.

start(SharedLib) ->
  case erl_ddll:load_driver(".", SharedLib) of
    ok -> ok;
    {error, already_loaded} -> ok;
    _ -> exit({error, could_not_load_driver})
  end,
  register_lib(SharedLib).

stop() ->
  [{port, Port}| _] = ets:lookup(haval_table, port),
  Port ! {close, self()},
  ok.

%% TODO: Implement this function return the information for module
info() -> ok.

%% Caluculate a value of HAVAL from string
haval_string(Str) -> 
 binary_to_term(control(?DRV_HAVAL_STRING, Str)).

%% Caluculate a value of HAVAL from file
haval_file(FileName) ->
  binary_to_term(control(?DRV_HAVAL_FILE, FileName)).

-ifdef(debug).
test() -> 
  haval:start(),
  Str = "I love Erlang.",
  FileName = "haval.erl",
  Hash1 = haval:haval_string(Str),
  Hash2 = haval:haval_file(FileName),
  io:format("HAVAL(~p) = ~p ~n", [Str, Hash1]),
  io:format("HAVAL(~p) = ~p ~n", [FileName, Hash2]),
  haval:stop(),
  halt().

test([Str|_]) ->
  haval:start(),
  Hash = haval:haval_string(Str),
  io:format("HAVAL(~p) = ~p ~n", [Str, Hash]),
  haval:stop(),
  halt().
-endif.

%%
%% Internal functions
%%
register_lib(SharedLib) ->
  Port = open_port({spawn, SharedLib}, []),
  Tab = ets:new(haval_table, [set, protected, named_table]),
  ets:insert(Tab, {port, Port}).

control(Cmd, Data) ->
  [{port, Port}| _] = ets:lookup(haval_table, port),
  erlang:port_control(Port, Cmd, Data).
