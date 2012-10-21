%% @copyright 2011 Johan Persson
%% @copytight 2012 Johan Persson, Programlabbet AB
%% @author Johan Persson <johan@programlabbet.se>
%%
%% @doc Development helper.
%%
-module(eyegor_app).
-author("Johan Persson <johan@programlabbet.se>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    eyegor_sup:start_link().

stop(_State) ->
    ok.
