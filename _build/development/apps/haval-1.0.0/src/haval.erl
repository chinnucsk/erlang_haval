%%% --------------------------------------------------------------------
%%% @doc    HAVAL bindings for Erlang
%%% @author Rakuto Furutani <xri://=rakuto/(+profile)>
%%% @end
%%% --------------------------------------------------------------------
-module(haval).
-author('Rakuto Furutani <xri://=rakuto/(+profile)>').

%% Functions
-define(DRV_INFO, 1).
-define(DRV_HAVAL_STRING, 2).
-define(DRV_HAVAL_FILE, 3).

-export([start/0, stop/0]).
-export([info/0, haval_string/1, haval_file/1]).

%% Unit Test
%% All functions whoes names match _test() or ..._test_() to be automatically exported.
-include("eunit.hrl").

%%
%% Public interface
%%
start() ->
  application:start(haval).

stop() ->
  application:stop(haval).

info() -> 
  [haval_string, haval_file].

%% Caluculate a value of HAVAL from string
haval_string(Str) -> 
 binary_to_term(control(?DRV_HAVAL_STRING, Str)).

%% Caluculate a value of HAVAL from file
haval_file(FileName) ->
  binary_to_term(control(?DRV_HAVAL_FILE, FileName)).

%%
%% Internal functions
%%
control(Cmd, Data) ->
  [{port, Port}| _] = ets:lookup(haval_server_table, port),
  erlang:port_control(Port, Cmd, Data).

%%
%% Unit Tests
%%
haval_string_test() ->
  haval:start(),
  Str = "I'm a fun of Erlang",
  ValidHash = "EDBCF3A39D3AEA64181B90EA2C30F830103FA5DEB790A6E6299C8F686743AA56",
  ?assert(haval:haval_string(Str) == ValidHash),
  haval:stop().

haval_file_test() ->
  haval:start(),
  File = filename:join([?FILE, "sample_file"]),
  ValidHash = "71EB15A01D6C592F0DE228F062AD3B91102FFD98A3D5C871AAF3FA20EF9EA377",
  ?assert(haval:haval_file(File) == ValidHash),
  haval:stop().
  
