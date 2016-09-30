-module(mafiapp).
-behaviour(application).

-export([install/1, start/2, stop/1, add_friend/4, add_service/4]).

-record(mafiapp_friends, {name,
  contact = [],
  info = [],
  expertise}).
-record(mafiapp_services, {from,
  to,
  date,
  description}).

start(normal, []) ->
  mnesia:wait_for_tables([mafiapp_friends,
    mafiapp_services], 5000),
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

%% Create the schema on the nodes specified in the Nodes list.
%% Then, we start Mnesia, which is a necessary step in order to create tables.
%% We create the two tables, named after the records #mafiapp_friends{} and #mafiapp_services{}.
%% There's an index on the expertise because we do expect to search friends by expertise in case of need, as mentioned earlier.
install(Nodes) ->
  ok = mnesia:create_schema(Nodes), %% Need to do it before starting Mnesia
  rpc:multicall(Nodes, application, start, [mnesia]), %% Need to start Mnesia on all of the Nodes
  mnesia:create_table(mafiapp_friends,
    [{attributes, record_info(fields, mafiapp_friends)},
      {index, [#mafiapp_friends.expertise]},
      {disc_copies, Nodes}]),
  mnesia:create_table(mafiapp_services,
    [{attributes, record_info(fields, mafiapp_services)},
      {index, [#mafiapp_services.to]},
      {disc_copies, Nodes},
      {type, bag}]),
  rpc:multicall(Nodes, application, stop, [mnesia]).