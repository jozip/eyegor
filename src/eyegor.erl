%% @copyright 2011 Johan Persson
%% @copytight 2012 Johan Persson, Programlabbet AB
%% @author Johan Persson <johan@programlabbet.se>
%%
%% @doc Application starter helper

-module(eyegor).
-author("Johan Persson <johan@programlabbet.se>").

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the eyegor server.
start() ->
    application:start(eyegor).


%% @spec stop() -> ok
%% @doc Stop the eyegor server.
stop() ->
    application:stop(eyegor).
