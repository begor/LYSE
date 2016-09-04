%%% Simple generic supervisor
%%% Can take any module, which has start_link function (1).
%%% It will restart the process (2) it watches indefinitely, unless
%%% the supervisor itself is terminated with a shutdown exit signal (3).
-module(sup).
-compile(export_all).


start(Mod, Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
  process_flag(trap_exit, true), % Need to catch those exit signals
  loop({Mod, start_link, Args}). % (1) We need module which has start_link function

%% @doc Main supervisor's loop. Starts child's processes and waits messages from them.
loop({M, F, A}) ->
  Pid = apply(M, F, A), % Applies a function F from a module M to arguments A.
  receive
    {'EXIT', _From, shutdown} -> % (3) Child was brutally killed, so will be a supervisor and all its childs.
      exit(shutdown);
    {'EXIT', Pid, Reason} -> % (2) Child is dead, just restart it.
      io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
      loop({M, F, A})
  end.
