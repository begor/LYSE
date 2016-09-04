%%% Simple generic server. Abstracts sync and async messages, main loop, termination, etc.
-module(my_server).
-export([call/2, cast/2, loop/2, reply/2]).

%% @doc Call a synchronous message Msg on process Pid.
call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

%% @doc Cast an asynchronous message Msg on process Pid.
cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

%% @doc Abstracts main loop of a process.
loop(Module, State) ->
  receive
    {sync, Pid, Ref, Msg} ->
      % Clients don't need to know about Refs, so we put {Pid, Ref} in a tuple From.
      NewState = Module:handle_call(Msg, {Pid, Ref}, State),
      loop(Module, NewState);
    {async, Msg} ->
      NewState = Module:handle_cast(Msg, State),
      loop(Module, NewState)
  end.

%% @doc Abstracts sending a reply.
reply({Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.