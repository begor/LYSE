-module(fixtures).
-include_lib("eunit/include/eunit.hrl").

double_register_test_() ->
  {"Verifies that the registry doesn't allow a single process to "
  "be registered under two names. We assume that each pid has the "
  "exclusive right to only one name", % testcase description
  {setup,
    fun start/0,               % setup function
    fun stop/1,                % teardown function
    fun two_names_one_pid/1}   % instantiator
  }.

start() ->
  {ok, Pid} = registry:start_link(),
  Pid.

stop(Pid) ->
  registry:stop(Pid).

% instantiator should return test set, tests from which will be run by EUnit
two_names_one_pid(Pid) ->
  ok = registry:register(Pid, quite_a_unique_name, self()),
  Res = registry:register(Pid, my_other_name_is_more_creative, self()),
  [?_assertEqual({error, already_named}, Res)].