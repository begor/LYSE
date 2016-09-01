%%% Events functionality
-module(events).
-author("egor").

%% API
-compile(export_all). % Just for easy compiling
-record(state, {server, name="", to_go=0}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, Name, Delay) ->
  NewDelay = normalize(Delay),
  loop(#state{server = Server, name = Name, to_go = NewDelay}).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid), % We need to check if event isn't already dead
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]), % No need to monitor it anymore, it's basically dead
      ok;
    {'DOWN', Ref, process, Pid, _Reason} -> % Already killed, just ok
      ok
  end.

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 -> % * 1000 = seconds
    case Next of % We slice big delays into chunks in normalize
      [] -> Server ! {done, S#state.name};
      _ -> loop(S=#state{to_go = Next})
    end
  end.

normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].