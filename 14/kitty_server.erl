%%% Naive handwritten implementation of a simple server.
-module(kitty_server).
-export([start_link/0, order/4, return/2, close_shop/1]).

-record(cat, {name, color = "red", desc}).

%%% API

%% @doc Spawns link with a server.
start_link() -> spawn_link(fun init/0).

%% @doc Orders a cat. Synchronous call (call that waits an answer to proceed).
order(Pid, Name, Color, Desc) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, {order, Name, Color, Desc}},
  receive
    {Ref, Cat} ->
      erlang:demonitor(Ref, [flush]),
      Cat;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 -> erlang:error(timeout)
  end.

%% @doc Returns a cat. Asynchronous call (call that doesn't wait an answer to proceed).
return(Pid, Cat) ->
  Pid ! {return, Cat},
  ok.

%% @doc Closes shop.
close_shop(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, terminate},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

%%% Implementation

init() ->
  loop([]).

loop(Cats) ->
  receive
    {Pid, Ref, {order, Name, Color, Desc}} ->
      case Cats of
        [C | Cs] -> Pid ! {Ref, C}, loop(Cs);
        [] -> Pid ! {Ref, make_cat(Name, Color, Desc)}, loop([])
      end;
    {return, Cat = #cat{}} ->
      loop([Cat | Cats]);
    {Pid, Ref, terminate} ->
      Pid ! {Ref, ok}, terminate(Cats);
    Unknown ->
      io:format("Server got unknown ~p~n", [Unknown]), loop(Cats)
  end.

make_cat(Name, Color, Desc) ->
  #cat{name = Name,
    color = Color,
    desc = Desc}.

terminate(Cats) ->
  lists:map(fun(Cat) -> io:format("Cat ~p is free!~n", [Cat]) end, Cats),
  ok.