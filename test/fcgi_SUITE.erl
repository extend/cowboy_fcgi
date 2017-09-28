%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(fcgi_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2]). %% ct.
-export([ping/1, hello/1, post/1, post_qs/1, redirect/1]). %% php-fpm.
-export([multiple/1, cookies/1]). %% headers.
-export([path_info_empty/1, path_info_slash/1,
	path_info_nonempty/1]). %% path info.

%% ct.

all() ->
	[{group, 'php-fpm'}].

groups() ->
	[
        {general, [], [ping, hello, post, post_qs, redirect]},
		{path_info, [], [path_info_empty, path_info_slash, path_info_nonempty]},
		{headers, [], [multiple, cookies]},
		{'php-fpm', [], [
			{group, general},
			{group, headers},
			{group, path_info}
		]}
	].

init_per_suite(Config) ->
	case application:load(ex_fcgi) of
		ok ->
			ok = application:start(inets),
			ok = application:start(crypto),
			ok = application:start(ranch),
			ok = application:start(cowlib),
			ok = application:start(cowboy),
			ok = application:start(ex_fcgi),
			Config;
		{error, _Reason} ->
			{skip, {notfound, ex_fcgi}}
	end.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(ex_fcgi),
	application:stop(inets),
	ok.

init_per_group('php-fpm', Config) ->
	case {os:find_executable("kill"), os:find_executable("php-fpm")} of
		{false, _} ->
			{skip, {notfound, kill}};
		{_, false} ->
			{skip, {notfound, 'php-fpm'}};
		{KillPath, FpmPath} ->
			TcpPort = 33080,
			DataDir = ?config(data_dir, Config),
			PrivDir = ?config(priv_dir, Config),
			os:cmd("\"" ++ FpmPath ++ "\" -y \"" ++ DataDir
				++ "\"/php-fpm.conf -p \"" ++ PrivDir ++ "\""),
			ex_fcgi:start('php-fpm', localhost, 33000),
			Opts = [{name, 'php-fpm'}, {script_dir, DataDir}],
			Dispatch = cowboy_router:compile([
 				{'_', [
					{"/ping", cowboy_http_fcgi, [{name, 'php-fpm'}]},
					{"/hello.php", cowboy_http_fcgi, Opts},
					{"/echo.php", cowboy_http_fcgi, Opts},
					{"/status.php", cowboy_http_fcgi, Opts},
					{"/redirect.php", cowboy_http_fcgi, Opts},
					{"/header.php", cowboy_http_fcgi, Opts},
					{"/cookies.php", cowboy_http_fcgi, Opts},
					{"/path_info.php/[...]", cowboy_http_fcgi, [
						{path_root, <<"/path/root">>}|Opts
					]}
				]}
			]),
			{ok, _} = cowboy:start_http(fcgi, 100,
				[{port, TcpPort}],
				[{env, [{dispatch, Dispatch}]}]
			),
			[{kill_path, KillPath}, {tcp_port, TcpPort}|Config]
	end;
init_per_group(_Group, Config) ->
	Config.

end_per_group('php-fpm', Config) ->
	cowboy:stop_listener(fcgi),
	ex_fcgi:stop('php-fpm'),
	KillPath = ?config(kill_path, Config),
	PidFile = filename:join([?config(priv_dir, Config), "php-fpm.pid"]),
	{ok, Pid} = file:read_file(PidFile),
	os:cmd("\"" ++ KillPath ++ "\" \"" ++ binary_to_list(Pid) ++ "\""),
	ok;
end_per_group(_Group, _Config) ->
	ok.

%% php-fpm.

ping(Config) ->
	Url = build_url("/ping", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "pong"}} =
		httpc:request(Url).

hello(Config) ->
	Url = build_url("/hello.php?name=Shinji", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers, "Shut up!\n"}} =
		httpc:request(Url),
	{"content-type", "text/plain;charset=UTF-8"} = lists:keyfind("content-type", 1, Headers).

post(Config) ->
	Body = "I'm a boring test post body.",
	Url = build_url("/echo.php", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}} =
		httpc:request(post, {Url, [], "text/x-plain-and-boring", Body}, [], []),
	{"content-type", "text/x-plain-and-boring;charset=UTF-8"} =
		lists:keyfind("content-type", 1, Headers),
	{"content-length", 28}.

post_qs(Config) ->
	Body = "world-state=ok&god-location=heaven",
	Url = build_url("/status.php", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Response}} =
		httpc:request(post,
			{Url, [], "application/x-www-form-urlencoded", Body}, [], []),
	"God's in his Heaven-- All's right with the world!" = Response.

redirect(Config) ->
	Url = build_url("/redirect.php", Config),
	HTTPOptions = [{autoredirect, false}],
	{ok, {{"HTTP/1.1", 302, _}, Headers, "Ping!\n"}} =
		httpc:request(get, {Url, []}, HTTPOptions, []),
	{"location", "http://localhost:33080/ping"} =
		lists:keyfind("location", 1, Headers).

%% path info.

path_info_empty(Config) ->
	Url = build_url("/path_info.php", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers, "ok\n"}} =
		httpc:request(Url),
	false = lists:keyfind("x-path-info", 1, Headers),
	{"x-path-translated", ""} = lists:keyfind("x-path-translated", 1, Headers).

path_info_slash(Config) ->
	Url = build_url("/path_info.php/", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers, "ok\n"}} =
		httpc:request(Url),
	{"x-path-info", "/"} = lists:keyfind("x-path-info", 1, Headers),
	{"x-path-translated", "/path/root/"} =
		lists:keyfind("x-path-translated", 1, Headers).

path_info_nonempty(Config) ->
	Url = build_url("/path_info.php/in/da/path", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers, "ok\n"}} =
		httpc:request(Url),
	{"x-path-info", "/in/da/path"} = lists:keyfind("x-path-info", 1, Headers),
	{"x-path-translated", "/path/root/in/da/path"} =
		lists:keyfind("x-path-translated", 1, Headers).

%% headers.

multiple(Config) ->
	Url = build_url("/header.php?name=X-Multiple-Header", Config),
	Request = {Url, [{"X-Multiple-Header", "1"}, {"X-Multiple-Header", "2"}]},
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "2,1"}} =
		httpc:request(get, Request, [], []).

cookies(Config) ->
	Url = build_url("/cookies.php?names=world-state,god-location", Config),
	ReqHeaders = [
		{"Cookie", "$Version=0; god-location=heaven"},
		{"Cookie", "world-state=ok"}
	],
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "ok\nheaven\n"}} =
		httpc:request(get, {Url, ReqHeaders}, [], []).

%% Internal.

build_url(Path, Config) ->
	"http://localhost:" ++ integer_to_list(?config(tcp_port, Config)) ++ Path.
