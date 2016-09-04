%%% Simple server for proceeding client's requests for events.
-module(server).
-compile(export_all).
-record(state, {events, clients}).
-record(event, {name = "", desc = "", pid, timeout = 10}).

%%% API.

%% @doc Starts a server.
%% Note Singleton-like hack: we need only one server running at a time,
%% so we register it with a name 'server'.
start() ->
  register(?MODULE, Pid = spawn(?MODULE, init, [])),
  Pid.

%% @doc Subscribes a client with a given PID.
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

%% @doc Adds an event with a name, description and delay.
add_event(Name, Desc, Delay) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Desc, Delay}},
  receive
    {Ref, ok} -> {ok, Ref}
  after 5000 ->
    {error, timeout}
  end.

%% @doc Cancels an event with a name, description and delay.
cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

%%% Implementation

%% @doc Initializes a main loop with empty dicts of events and clients.
init() ->
  loop(#state{events = orddict:new(),
    clients = orddict:new()}).

%% @doc Main loop. Takes a single state argument which holds dicts with
%% subscribed clients and registered events.
loop(S = #state{clients = Clients, events = Events}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      % Monitor: The only other time we’ll need
      % to fetch the client ID will be if we receive a monitor’s
      % EXIT message, which will contain the reference.
      Ref = erlang:monitor(process, Client),
      NewClient = orddict:store(Ref, Client, Clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients = NewClient});
    {Pid, MsgRef, {add, Name, Desc, TimeOut}} ->
      % Check that given timeout is a valid time, and then
      % creates an event, and store it in state's dict events.
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
      % Finds an event in state's dict with events and cancels it via events:cancel func.
      NewEvents = case orddict:find(Name, Events) of
                    {ok, E} -> events:cancel(E#event.pid),
                      orddict:erase(E, Events);
                    error -> Events
                  end,
      Pid ! {MsgRef, ok},
      loop(S#state{events = NewEvents});
    {done, Name} ->
      % Finds an event in state's dict with events and sends information about it to clients.
      case orddict:find(Name, Events) of
        {ok, E} ->
          send_to_clients(E, Clients), % Notify all the clients in Clients dict
          NewEvents = orddict:erase(E, Events),
          loop(S#state{events = NewEvents});
        error ->
          loop(S)
      end
  end.

%% @doc Here should be check for validity of time, but it's not so important.
valid_time(_) -> true.

%% @doc Sends ALL the clients in Clients dict message that E-event is done.
send_to_clients(E, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! {done, E#event.name} end, Clients).