%%% -----------------------------------------------------------------
%%% @doc  HAVAL bindings for Erlang
%%% @end
%%% -----------------------------------------------------------------
-module(haval_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Constant
-define(SERVER, ?MODULE).

%% ================================================================
%% API
%% ================================================================
%% ----------------------------------------------------------------
%% Func:  start_link() -> {ok, Pid} | ignore | {error, Error}
%% Desc:  Starts the server
%% ----------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ================================================================
%% gen_server callbacks
%% ================================================================

%% ----------------------------------------------------------------
%% Func:  init(Args) -> {ok, State} }
%%                      {ok, State, Timeout} |
%%                      ignore |
%%                      {stop, Reason}
%%
%% Desc:  Initiates the server
%% ----------------------------------------------------------------

init([]) -> 
  process_flag(trap_exit, true),
  erl_ddll:start(),
  PrivDir = case code:priv_dir(haval) of 
    {error, bad_name} ->
      Dir = filename:dirname(?FILE),
      filename:join([Dir, "..", "priv"]);
    Path -> Path
  end,
  LibDir = filename:join([PrivDir, "lib"]),
  case erl_ddll:load_driver(LibDir, haval_drv) of
    ok -> ok;
    {error, already_loaded} -> ok;
    _ -> exit({error, could_not_load_driver})
  end,
  register_lib(haval_drv).


%% ----------------------------------------------------------------
%% The gen_server callback functions
%% ----------------------------------------------------------------
handle_call(_, _, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_INfo, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, {Port, _}) ->
  Port ! {close, self()},
  ok.
  

%% ----------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------
register_lib(SharedLib) ->
  Port  = open_port({spawn, SharedLib}, []),
  %% Register a port in ets
  Tab = ets:new(haval_server_table, [set, protected, named_table]),
  ets:insert(Tab, {port, Port}),
  {ok, {Port, []}}.
