%%% Supervisor for each process pool.
-module(ppool_sup).
-behaviour(supervisor).

-export([init/1]). % Supervisor

start_link(Name, Limit, MFA) ->
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).

%%% Supervisor

init({Name, Limit, MFA}) ->
  MaxRestart = 1,
  MaxTime = 3600,
  
  %% Note that the Name is passed to the server, along with self(),
  %% the supervisor’s own pid.
  %% This will let the server call for the spawning of the worker supervisor;
  %% the MFA variable will be used in that call to let the
  %% simple_one_for_one supervisor know which kind of workers to run.
  {ok, {{one_for_all, MaxRestart, MaxTime},
        [{serv,
          {ppool_serv, start_link, {Name, Limit, self(), MFA}},
          permanent,
          5000,
          worker,
          [ppool_serv]}]}}.