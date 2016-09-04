%%% Naive handwritten implementation of a simple server.
-module(kitty_server).
-export([start_link/0, order/4, return/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]). % For my_server support

-record(cat, {name, color = "red", desc}).

%%% API

%% @doc Spawns link with a server.
start_link() -> my_server:start_link(?MODULE, []).

%% @doc Orders a cat. Synchronous call (call that waits an answer to proceed).
order(Pid, Name, Color, Desc) ->
  my_server:call(Pid, {order, Name, Color, Desc}).

%% @doc Returns a cat. Asynchronous call (call that doesn't wait an answer to proceed).
return(Pid, Cat) ->
  my_server:cast(Pid, {return, Cat}).

%% @doc Closes shop.
close_shop(Pid) ->
  my_server:call(Pid, terminate).

%%% Implementation

init([]) -> [].

handle_call({order, Name, Color, Desc}, From, Cats) ->
  case Cats of
    [C | Cs] ->
      my_server:reply(From, C),
      Cs;
    [] ->
      my_server:reply(From, make_cat(Name, Color, Desc)),
      Cats
  end;
handle_call(terminate, From, Cats) ->
  my_server:reply(From, ok),
  terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
  [Cat | Cats].

make_cat(Name, Color, Desc) ->
  #cat{name = Name,
    color = Color,
    desc = Desc}.

terminate(Cats) ->
  lists:map(fun(Cat) -> io:format("Cat ~p is free!~n", [Cat]) end, Cats),
  ok.