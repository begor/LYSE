-module(erlcount_dispatch).
-behaviour(gen_fsm).
-export([start_link/0, complete/4]).
-export([init/1, dispatching/2, listening/2, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(POOL, erlcount).

-record(data, {regex=[], refs=[]}). % To keep track of running workers

%%% API

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
  gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).

%%% Callbacks

init([]) ->
  %% Move the get_env stuff to the supervisor's init.
  {ok, Re} = application:get_env(regex),
  {ok, Dir} = application:get_env(directory),
  {ok, MaxFiles} = application:get_env(max_files),
  %% Starting the process pool with erlcount_counter as a callback module.
  ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
  case lists:all(fun valid_regex/1, Re) of
    true ->
      self() ! {start, Dir},
      Regexes = [{R, 0} || R <- Re],
      {ok, dispatching, #data{regex=Regexes}};
    false ->
      {stop, invalid_regex}
  end.

handle_info({start, Dir}, State, Data) ->
  % We are in "dispatching" state and send event from CPS (see find_erl)
  gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
  {next_state, State, Data}.

handle_event({complete, Regex, Ref, Count}, State, Data = #data{regex=Re, refs=Refs}) ->
  {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
  NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount+Count}), % and update it with the new count (OldCount+Count)
  NewData = Data#data{regex=NewRe, refs=Refs--[Ref]},
  case State of
    dispatching ->
      {next_state, dispatching, NewData};
    listening ->
      listening(done, NewData) % we have to manually call listening/2 again
  end.

handle_sync_event(Event, _From, State, Data) ->
  io:format("Unexpected event: ~p~n", [Event]),
  {next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
  init:stop().

code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%% States

%% CPS event received
%% For each of the regular expressions, we create a unique reference,
%% schedule a ppool worker that knows this reference, and then store this reference (to know if a worker has finished).
%% Once that dispatching is done, we call the continuation again to get more results,
%% and then wait for the next message with the new references as our state.
dispatching({continue, File, Continuation}, Data = #data{regex=Re, refs=Refs}) ->
  F = fun({Regex, _Count}, NewRefs) ->
        Ref = make_ref(),
        ppool:async_queue(?POOL, [self(), Ref, File, Regex]), % Schedule async worker in ppool
        [Ref|NewRefs]
      end,
  NewRefs = lists:foldl(F, Refs, Re),
  gen_fsm:send_event(self(), Continuation()), % Run continuation to get more results
  {next_state, dispatching, Data#data{refs = NewRefs}};
%% This is a special case. We can not assume that all messages have NOT
%% been received by the time we hit 'done'. As such, we directly move to
%% listening/2 without waiting for an external event.
dispatching(done, Data) ->
  listening(done, Data).

%% If no refs are left, then everything was received and we can output the results.
%% Otherwise, we can keep listening to messages.
listening(done, #data{regex=Re, refs=[]}) ->
  [io:format("Regex ~s has ~p results~n", [R,C]) || {R, C} <- Re],
  {stop, normal, done};
listening(done, Data) -> % entries still missing
  {next_state, listening, Data}.


valid_regex(Re) ->
  try re:run("", Re) of
    _ -> true
  catch
    error:badarg -> false
  end.

