-module(mafiapp).
-behaviour(application).
-include_lib("stdlib/include/ms_transform.hrl").

-export([install/1, start/2, stop/1, add_friend/4, add_service/4, friend_by_name/1, friend_by_expertise/1, debts/1, add_enemy/2, find_enemy/1, enemy_killed/1]).

-record(mafiapp_friends, {name,
  contact = [],
  info = [],
  expertise}).
-record(mafiapp_services, {from,
  to,
  date,
  description}).
-record(mafiapp_enemies, {name,
  info = []}).

start(normal, []) ->
  mnesia:wait_for_tables([mafiapp_friends,
    mafiapp_services,
    mafiapp_enemies], 5000),
  mafiapp_sup:start_link().

stop(_) -> ok.

add_friend(Name, Contact, Info, Expertise) ->
  F = fun() ->
    mnesia:write(#mafiapp_friends{name = Name,
      contact = Contact,
      info = Info,
      expertise = Expertise})
      end,
  mnesia:activity(transaction, F).

friend_by_name(Name) ->
  F = fun() ->
    case mnesia:read({mafiapp_friends, Name}) of
      [#mafiapp_friends{contact = C, info = I, expertise = E}] ->
        {Name, C, I, E, find_services(Name)};
      [] ->
        undefined
    end
      end,
  mnesia:activity(transaction, F).

%% whenever From matches Name we return a {to, ToName, Date, Description} tuple.
%% Whenever Name matches To instead, the function returns a tuple of the form {from, FromName, Date, Description},
%% allowing us to have a single operation that includes both services given and received.
%% find_services/1 does not run in any transaction.
%% That's because the function is only called within friend_by_name/1, which runs in a transaction already.
find_services(Name) ->
  Match = ets:fun2ms(
    fun(#mafiapp_services{from = From, to = To, date = D, description = Desc})
      when From =:= Name ->
      {to, To, D, Desc};
      (#mafiapp_services{from = From, to = To, date = D, description = Desc})
        when To =:= Name ->
        {from, From, D, Desc}
    end
  ),
  mnesia:select(mafiapp_services, Match).

friend_by_expertise(Expertise) ->
  Pattern = #mafiapp_friends{_ = '_',
    expertise = Expertise},
  F = fun() ->
    Res = mnesia:match_object(Pattern),
    [{Name, C, I, Expertise, find_services(Name)} ||
      #mafiapp_friends{name = Name,
        contact = C,
        info = I} <- Res]
      end,
  mnesia:activity(transaction, F).

add_service(From, To, Date, Description) ->
  F = fun() ->
    case mnesia:read({mafiapp_friends, From}) =:= [] orelse mnesia:read({mafiapp_friends, To}) =:= [] of
      true ->
        {error, unknown_friend}; %% No "relations" found
      false ->
        mnesia:write(#mafiapp_services{from = From,
          to = To,
          date = Date,
          description = Description})
    end
      end,
  mnesia:activity(transaction, F).

debts(Name) ->
  Match = ets:fun2ms(
    fun(#mafiapp_services{from = From, to = To}) when From =:= Name ->
      {To, -1};
      (#mafiapp_services{from = From, to = To}) when To =:= Name ->
        {From, 1}
    end),
  F = fun() -> mnesia:select(mafiapp_services, Match) end,
  Dict = lists:foldl(fun({Person, N}, Dict) ->
    dict:update(Person, fun(X) -> X + N end, N, Dict)
                     end,
    dict:new(),
    mnesia:activity(transaction, F)),
  lists:sort([{V, K} || {K, V} <- dict:to_list(Dict)]).

add_enemy(Name, Info) ->
  F = fun() -> mnesia:write(#mafiapp_enemies{name=Name, info=Info}) end,
  mnesia:activity(transaction, F).

find_enemy(Name) ->
  F = fun() -> mnesia:read({mafiapp_enemies, Name}) end,
  case mnesia:activity(transaction, F) of
    [] -> undefined;
    [#mafiapp_enemies{name=N, info=I}] -> {N,I}
  end.

enemy_killed(Name) ->
  F = fun() -> mnesia:delete({mafiapp_enemies, Name}) end,
  mnesia:activity(transaction, F).

%% Create the schema on the nodes specified in the Nodes list.
%% Then, we start Mnesia, which is a necessary step in order to create tables.
%% We create the two tables, named after the records #mafiapp_friends{} and #mafiapp_services{}.
%% There's an index on the expertise because we do expect to search friends by expertise in case of need, as mentioned earlier.
install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  application:start(mnesia),
  mnesia:create_table(mafiapp_friends,
    [{attributes, record_info(fields, mafiapp_friends)},
      {index, [#mafiapp_friends.expertise]},
      {disc_copies, Nodes}]),
  mnesia:create_table(mafiapp_services,
    [{attributes, record_info(fields, mafiapp_services)},
      {index, [#mafiapp_services.to]},
      {disc_copies, Nodes},
      {type, bag}]),
  mnesia:create_table(mafiapp_enemies,
    [{attributes, record_info(fields, mafiapp_enemies)},
      {disc_copies, Nodes},
      {local_content, true}]),
  application:stop(mnesia).