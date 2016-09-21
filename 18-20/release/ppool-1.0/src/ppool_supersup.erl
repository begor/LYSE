%%% Top supervisor.
-module(ppool_supersup).
-behaviour(supervisor).

-export([start_link/0, stop/0, start_pool/3, stop_pool/1]). % API
-export([init/1]). % Supervisor

%%% API

start_link() ->
  % we gave the top-level process pool supervisor the name ppool.
  supervisor:start_link({local, ppool}, ?MODULE, []).

%% Technically, a supervisor cannot be killed in an easy way.
%% Let's do it brutally!
stop() ->
  case whereis(ppool) of
    P when is_pid(P) -> exit(P, kill);
    _ -> ok
  end.

%% @doc Builds child specification and
%% starts a pool supervisor under ppool_supersup.
start_pool(Name, Limit, MFA) ->
  ChildSpec = {Name,
               {ppool_sup, start_link, [Name, Limit, MFA]},
               permanent, 10500, supervisor, [ppool_sup]},
  supervisor:start_child(ppool, ChildSpec).

%% @doc Stops a child of ppool supersup with a Name.
stop_pool(Name) ->
  supervisor:terminate_child(ppool, Name),
  supervisor:delete_child(ppool, Name).

%%% Supervisor implementation

init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.