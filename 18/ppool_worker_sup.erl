%%% Supervisor for pool's workers.
-module(ppool_worker_sup).
-export([start_link/1, init/1]).
-behavior(supervisor).


start_link(MFA = {_,_,_}) ->
  supervisor:start_link(?MODULE, MFA).

%% @doc Picked a simple_one_for_one supervisor because workers
%% could be added in very high numbers with a requirement for speed,
%% plus we want to restrict their type.
%% All the workers are temporary, and because we use an {M,F,A}
%% tuple to start the worker, we can use any kind of OTP behavior there.
init({M,F,A}) ->
  MaxRestart = 5,
  MaxTime = 3600,
  {ok, {{simple_one_for_one, MaxRestart, MaxTime},
    [{ppool_worker,
      {M,F,A},
      temporary, 5000, worker, [M]}]}}.
