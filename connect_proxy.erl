-module(connect_proxy).
-export([listen/1]).

listen(Port) ->
	case gen_tcp:listen(Port, [ binary, 
															{packet, 0},
															{active, false} ]) of

		{ ok, ListenSocket } ->
			io:format("Listen to ~w.~n", [ Port ]),	
			server_loop(ListenSocket);

		{ error, Reason } ->
			io:format("Fail to listen. Reason: ~w.~n", [ Reason ])
	end.

server_loop(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		
		{ok, Socket} ->
			spawn(fun() -> socket_loop(Socket) end),
			server_loop(ListenSocket)
	end.

socket_loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Binary} ->
			gen_tcp:send(Socket, Binary),
			socket_loop(Socket);
	
		{error, _} ->
			gen_tcp:close(Socket)
	end.