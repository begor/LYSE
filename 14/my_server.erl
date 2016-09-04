%%% Simple generic server. Abstracts sync and async messages, main loop, termination, etc.
-module(my_server).
-export([call/2]).

%% @doc Call a synchronous message Msg on process Pid.
call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.


