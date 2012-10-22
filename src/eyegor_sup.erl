%% @copyright 2011 Johan Persson
%% @copytight 2012 Johan Persson, Programlabbet AB
%% @author Johan Persson <johan@programlabbet.se>
%%
%% @doc Supervisor

-module(eyegor_sup).
-author("Johan Persson <johan@programlabbet.se>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CHILD(reloader, worker),
                                  ?CHILD(recompiler, worker)
                                 ]} }.

