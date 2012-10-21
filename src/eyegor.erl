%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc eyegor.

-module(eyegor).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the eyegor server.
start() ->
    application:start(eyegor).


%% @spec stop() -> ok
%% @doc Stop the eyegor server.
stop() ->
    application:stop(eyegor).
