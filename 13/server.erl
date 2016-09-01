%%% Simple server
-module(server).
-compile(export_all).
-record(state, {events, clients}).
-record(event, {name="", desc="", pid, timeout=10}).

%%% Public interface hiding actual message passing.

start() ->
  % Singleton-like hack: we need only one server running at a time.
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} -> {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Desc, Delay) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Desc, Delay}},
  receive
    {Ref, ok} -> {ok, Ref}
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

%%% Implementation

init() ->
  loop(#state{events = orddict:new(),
              clients = orddict:new()}).

loop(S = #state{clients = Clients, events = Events}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      % Monitor: The only other time we’ll need
      % to fetch the client ID will be if we receive a monitor’s
      % EXIT message, which will contain the reference
      Ref = erlang:monitor(process, Client),
      NewClient = orddict:store(Ref, Client, Clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients = NewClient});
    {Pid, MsgRef, {add, Name, Desc, TimeOut}} ->
      case valid_time(TimeOut) of
        true ->
          EventPid = events:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name, #event{name = Name,
                                                 desc = Desc,
                                                 pid = EventPid,
                                                 timeout = TimeOut},
                                    Events),
          Pid ! {MsgRef, ok},
          loop(S#state{events = NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_time}},
          loop(S)
      end;
    {Pid, MsgRef, {cancel, Name}} ->
      NewEvents = case orddict:find(Name, Events) of
                    {ok, E} -> events:cancel(E#event.pid),
                               orddict:erase(E, Events);
                    error -> Events
                  end,
      Pid ! {MsgRef, ok},
      loop(S#state{events = NewEvents});
    {done, Name} ->
      case orddict:find(Name, Events) of
        {ok, E} ->
          send_to_clients(E, Clients), % Notify all the clients in Clients dict
          NewEvents = orddict:erase(E, Events),
          loop(S#state{events = NewEvents});
        error ->
          loop(S)
      end
  end.

valid_time(TimeOut) -> true.  %% Need to implement

send_to_clients(E, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! {done, E#event.name} end, Clients).