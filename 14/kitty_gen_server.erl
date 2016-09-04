%%% Simple implementation of a server using OTP's gen_server.
-module(kitty_gen_server).
-behaviour(gen_server).

%% API
-export([start_link/0, order/4, return/2, close/1]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(cat, {name, color = "red", desc}).


%%% API


start_link() ->
  % The first parameter is the callback module, the second one is a term
  % to pass to init/1 , and the third one is about debugging options for running servers.
  gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order(Pid, Name, Color, Desc) ->
  gen_server:call(Pid, {order, Name, Color, Desc}).

%% This call is asynchronous.
return(Pid, Cat) ->
  gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close(Pid) ->
  gen_server:call(Pid, terminate).


%%% Server functions


init([]) ->
  {ok, []}.

handle_call({order, Name, Color, Desc}, _From, Cats) ->
  case Cats of
    [C | Cs] -> {reply, C, Cs};
    [] -> {reply, make_cat(Name, Color, Desc), Cats}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, Cats}.

terminate(notmal, Cats) ->
  lists:map(fun(Cat) -> io:format("Cat ~p is free!~n", [Cat]) end, Cats),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

make_cat(Name, Color, Desc) ->
  #cat{name = Name,
    color = Color,
    desc = Desc}.