% Copyright Fuji Xerox Co., Ltd. 2013-2014 All rights reserved.

% If domain of target server is defined in white list, tis proxy server provides SSH connectivity
% between internal client and external server using HTTP connect method.
% i.e. client <-SSH over HTTP-> this proxy <-SSH over HTTP-> upstream proxy <-SSH-> external ssh server.

% Otherwise, this server relays HTTP packets to internal git server.
% i.e. client <-HTTP-> this proxy <-HTTP-> internal git server.

-module(connect_proxy).
-export([start/1]).

-define(HTTP_OPTIONS, [list,   {packet, 0}, {active, false}, {reuseaddr, true}, {packet,http}]).
-define(TCP_OPTIONS,  [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

http_proxy() ->
	case os:getenv("http_proxy") of
		false ->
			{ok, {"localhost", 8080}};
		Proxy ->
			case http_uri:parse(Proxy) of
				{_, _, Host, Port, _, _} ->
					{ok, {Host, Port}};
				_ ->
					{ok, {"localhost", 8080}}
			end
	end.

is_white(Host) ->
	{ok, WhiteList} = file:consult("whitelist"),
	case lists:keyfind(Host, 1, WhiteList) of
		false ->
			false;
		_ ->
			true
	end.

start(Port) ->
	{ok, LSock} = gen_tcp:listen(Port, ?HTTP_OPTIONS),
	do_accept(LSock).

do_accept(LSock) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	gen_tcp:controlling_process(Sock, spawn(fun() -> socket_handle(Sock) end)),
	do_accept(LSock).

socket_handle(Sock) ->
	case parse_request(Sock) of
		{ok, {"CONNECT", Host, Port, Headers}} ->
			io:format("~p~n", [Headers]),
			do_ssl_proxy(Sock, Host, Port);
		{ok, {Method, Host, Port, Headers}} ->
			gen_tcp:close(Sock);
		_ ->
			gen_tcp:close(Sock)
	end.

parse_request(Sock) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, {http_request, Method, {scheme, Host, PortStr}, _Version}} ->
			{Port, _} = string:to_integer(PortStr),
			{ok, Headers} = parse_headers(Sock),
			{ok, {Method, Host, Port, Headers}}
	end.

parse_headers(Sock) ->
	parse_headers(Sock, []).
parse_headers(Sock, Headers) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, http_eoh} ->
			{ok, Headers};
		{ok, {http_header, _, Header, _, Value}} ->
			parse_headers(Sock, [{Header, Value}|Headers]);
		{ok, {http_error, Reason}} ->
			throw(Reason);
		{error, Reason} ->
			throw(Reason)
	end.

do_ssl_proxy(Sock, Host, Port) ->
	printConnectLog(Host, Port),

	IsWhite = is_white(Host),
	case IsWhite of
		true ->
			{ok, {TargetHost, TargetPort}} = http_proxy();
		false ->
			TargetHost = Host,
			TargetPort = Port
	end,

	inet:setopts(Sock, ?TCP_OPTIONS),
	{ok, Proxy} = gen_tcp:connect(TargetHost, TargetPort, ?TCP_OPTIONS),

	case IsWhite of
		true ->
			gen_tcp:send(Proxy, lists:flatten(io_lib:format("CONNECT ~s:~s HTTP/1.0\r\n\r\n", [Host, Port])));
		false ->
			gen_tcp:send(Sock, "HTTP/1.0 200 Connection established\r\n\r\n")
	end,
	
	gen_tcp:controlling_process(Sock,  spawn(fun() -> pipe(Sock,  Proxy) end)),
	gen_tcp:controlling_process(Proxy, spawn(fun() -> pipe(Proxy, Sock)  end)).

pipe(FromSock, ToSock) ->
	case gen_tcp:recv(FromSock, 0) of
		{ok, Data} ->
			gen_tcp:send(ToSock, Data),
			pipe(FromSock, ToSock);
		{error, _} ->
			gen_tcp:close(FromSock),
			gen_tcp:close(ToSock)
	end.

printConnectLog(Host, Port) ->
	io:format("connect to ~p~n", [{Host, Port}]).

