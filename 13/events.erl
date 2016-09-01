-module(events).
-author("egor").

%% API
-compile(export_all).
-record(state, {server, name="", to_go=0}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, Name, Delay) ->
  NewDelay = normalize(Delay),
  io:format("Delay: ~p~n", [NewDelay]),
  loop(#state{server = Server, name = Name, to_go = NewDelay}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
       ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 ->
    case Next of
      [] -> Server ! {done, S#state.name};
      _ -> loop(S=#state{to_go = Next})
    end
  end.

normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].