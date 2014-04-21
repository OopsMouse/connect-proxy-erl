-module(connect_proxy_tests).
-include_lib("eunit/include/eunit.hrl").

% http_proxy
http_proxy_positive_1_test() ->
	{ok, {Host, Port}} = connect_proxy:http_proxy("http://localhost:3000"),
	?assert(Host =:= "localhost"),
	?assert(Port =:= 3000).

http_proxy_positive_2_test() ->
	{ok, {Host, Port}} = connect_proxy:http_proxy("http://proxy.co.jp:8080"),
	?assert(Host =:= "proxy.co.jp"),
	?assert(Port =:= 8080).

http_proxy_positive_3_test() ->
	Result = connect_proxy:http_proxy("hogehoge"),
	?assert(Result =:= undefined).

http_proxy_positive_4_test() ->
	Result = connect_proxy:http_proxy(false),
	?assert(Result =:= undefined).

% is_white
is_white_positive_1_test() ->
	IsWhite = connect_proxy:is_white("example.com"),
	?assert(IsWhite =:= true).

is_white_positive_2_test() ->
	IsWhite = connect_proxy:is_white("github.com"),
	?assert(IsWhite =:= false).