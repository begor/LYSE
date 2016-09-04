%%% Simple supervisor implementation.
-module(band_supervisor).
-author("egor").
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

%% @doc Starts a link with supervisor of a given Type.
start_link(Type) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

%% @doc The band supervisor will allow its band members to make a few
%% mistakes before shutting down all operations, based on what
%% mood he's in. A lenient supervisor will tolerate more mistakes
%% than an angry supervisor, who'll tolerate more than a
%% complete jerk supervisor.
init(lenient) ->
  init({one_for_one, 3, 60}); %% 4 failures in 60 secs means the end.
init(angry) ->
  %% fires the one that failed, and the others after him. 2 failures in 60 secs means the end.
  init({rest_for_one, 2, 60});
init(jerk) ->
  %% fires all if someone failed. 1 failure in 60 secs means the end.
  init({one_for_all, 1, 60});
init({RestartStrategy, MaxRestart, MaxTime}) ->
  {ok, {{RestartStrategy, MaxRestart, MaxTime},
    [{singer,
      {musicians, start_link, [singer, good]}, %% {Module, Fun, Args} to restarts
      permanent, 1000, worker, [musicians]},
      {bass,
        {musicians, start_link, [bass, good]},
        temporary, 1000, worker, [musicians]},
      {drum,
        {musicians, start_link, [drum, bad]},
        transient, 1000, worker, [musicians]},
      {keytar,
        {musicians, start_link, [keytar, good]},
        transient, 1000, worker, [musicians]}
    ]}}.