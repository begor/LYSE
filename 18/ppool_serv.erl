-module(ppool_serv).
-behaviour(gen_server).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  code_change/3, terminate/2]).

%% Needed to start "friendly" worker supervisor dynamically
-define(SPEC(MFA),
  {worker_sup,
    {ppool_worker_sup, start_link, [MFA]},
    temporary,
    10000,
    supervisor,
    [ppool_worker_sup]}).

-record(state, {limit=0,
  sup,
  refs,
  queue=queue:new()}).

%%% API

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
  gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
  gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
  gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
  gen_server:cast(Name, {async, Args}).

stop(Name) ->
  gen_server:call(Name, stop).

%%% Callbacks

init({Limit, MFA, Sup}) ->
  %% We need to find the Pid of the worker supervisor from here,
  %% but alas, this would be calling the supervisor while it waits for us!
  self() ! {start_worker_supervisor, Sup, MFA},
  {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

%% There is a space (N > 0), so we start new worker in a pool
handle_call({run, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}}; % Note storing monitor in State.
%% No more space for worker in pool!
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
  {reply, noalloc, S};
%% Same as first clause with "run" message
handle_call({sync, Args}, _From, S = #state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {reply, {ok,Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
%% Rather than replying with noalloc as we did last time, this one doesn't reply to the caller,
%% keeps the From information and enqueues it for a later time (see *) when there is space for the worker to be run
handle_call({sync, Args},  From, S = #state{queue=Q}) ->
  {noreply, S#state{queue=queue:in({From, Args}, Q)}};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

%% Again, the same.
handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
  {ok, Pid} = supervisor:start_child(Sup, Args),
  Ref = erlang:monitor(process, Pid),
  {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref,R)}};
%% No more space, no one to reply, just send it to the queue
handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 ->
  {noreply, S#state{queue=queue:in(Args,Q)}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% Cause we using monitors, we receive 'DOWN' messages.
%% That means, some worker (with Ref) is over, so we should remove it from refs and free the room for new one.
handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{refs=Refs}) ->
  io:format("received down msg~n"),
  case gb_sets:is_element(Ref, Refs) of
    true ->
      handle_down_worker(Ref, S);
    false -> %% Not our responsibility
      {noreply, S}
  end;
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
  {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
  link(Pid),
  {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.


%%% Inner funtions

%% Called when received 'DOWN' message.
%% Gets last element from queue.
%% If exists, starts it (note sync and async) and removes dead one from refs.
%% If queue is empty, just increment free counter and remove dead one.
handle_down_worker(Ref, S = #state{limit=L, sup=Sup, refs=Refs}) ->
  case queue:out(S#state.queue) of
    {{value, {From, Args}}, Q} ->
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
      gen_server:reply(From, {ok, Pid}), % * Now we finally can reply.
      {noreply, S#state{refs=NewRefs, queue=Q}};
    {{value, Args}, Q} ->
      {ok, Pid} = supervisor:start_child(Sup, Args),
      NewRef = erlang:monitor(process, Pid),
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
      {noreply, S#state{refs=NewRefs, queue=Q}};
    {empty, _} ->
      {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref,Refs)}}
  end.