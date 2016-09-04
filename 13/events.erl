%%% Events functionality
-module(events).
-compile(export_all). % Just for easy compiling
-record(state, {server, name = "", to_go = 0}).

%%% API

%% @doc Starts an event with name and delay.
start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

%% @doc Starts link for an event with name and delay.
start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

%% @doc Cancels an event with specific PID.
cancel(Pid) ->
  Ref = erlang:monitor(process, Pid), % We need to check if event isn't already dead
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]), % No need to monitor it anymore, it's basically dead
      ok;
    {'DOWN', Ref, process, Pid, _Reason} -> % Already killed, it's ok for us
      ok
  end.

%%% Implementation

%% @doc Initializes an event and binds it to a specific server.
init(Server, Name, Delay) ->
  NewDelay = normalize(Delay),
  loop(#state{server = Server, name = Name, to_go = NewDelay}).

%% @doc Main event loop.
%% The only message that event can accept is about cancelling.
%% After to_go list of delays it sends its server a message about finishing.
loop(S = #state{server = Server, to_go = [T | Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T * 1000 -> % * 1000 = seconds
    case Next of % We slice big delays into chunks in normalize
      [] -> Server ! {done, S#state.name};
      _ -> loop(S = #state{to_go = Next})
    end
  end.

%% @doc Max waiting time in after-clause is 49 days, so we
%% split delays bigger in chunks each of which <= 49 days.
normalize(N) ->
  Limit = 49 * 24 * 60 * 60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].