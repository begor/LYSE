-module(naive_tcp).
-compile(export_all).

% Starts listen socket, starts first acceptor with it and idles forever (to keep listen socket alive)
start_server(Port) ->
  Pid = spawn_link(
    fun() ->
      {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
      spawn(fun() -> acceptor(Listen) end),
      timer:sleep(infinity)
    end),
  {ok, Pid}.

% Accepts and replace itself with a fresh new accepting socket.
acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handle(Socket)
  end.