-module(sockserv_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Port} = application:get_env(port),
  %% Set the socket into {active_once} mode.
  %% See sockserv_serv comments for more details
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),

  % Inside a spawn_link/1 call for a simple reason:
  % the supervisor process is in its init/1 phase and cannot answer any messages.
  % If we were to call ourselves from within the init function, the process would deadlock and never finish running.
  % An external process is needed just for this reason.
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
    [{socket,
      {sockserv_serv, start_link, [ListenSocket]}, % pass the socket!
      temporary, 1000, worker, [sockserv_serv]}
    ]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.