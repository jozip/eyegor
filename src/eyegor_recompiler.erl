%% @copyright 2011-2012 Johan Persson
%% @author Johan Persson <johan@programlabbet.se>
%%
%% @doc Erlang module for automatically recompiling modified modules
%% during development
%%
%% Based heavily on reloader.erl by Matthew Dempsky.

-module(eyegor_recompiler).
-author("Johan Persson <johan@programlabbet.se>").

-behaviour(gen_server).

-export([start_link/0, start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%====================================================================
%% Includes and definitions
%%====================================================================

-include_lib("kernel/include/file.hrl").

-record(state, {last, tref}).

-define(SERVER, ?MODULE).


%%====================================================================
%% External functions
%%====================================================================

%% @spec start_link() -> ServerRet
%% @doc Starts the recompiler
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec start() -> ServerRet
%% @doc Starts the recompiler
start() ->
    start_link().

%% @spec stop() -> ok
%% @doc Stops the recompiler
stop() ->
    gen_server:call(?SERVER, stop).


%%====================================================================
%% Server functions
%%====================================================================

%% @spec init([]) -> tuple()
%% @doc Initiates the recompiler
init([]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
    {ok, #state{last = erlang:localtime(), tref = TRef}}.

%% @spec handle_call(Arg, From, State) -> tuple()
%% @doc Handling call messages
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Msg, State) -> tuple()
%% @doc Handling cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc Handling all non call/cast messages
handle_info(doit, State) ->
    Now = erlang:localtime(),
    doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> any
%% @doc Shutdown the server
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @spec doit(From, To) -> any
%% @doc Preforms an operation if a file has mtime between From and To
doit(From, To) ->
    [case filename:find_src(Module) of
         {error, Reason} ->
             Reason;
         {Filename, Options} -> 
             case file:read_file_info(Filename ++ ".erl") of
                 {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
                     recompile(Module, Filename,
                               lists:keyreplace(outdir, 1, Options,
                                                {outdir, filename:dirname(code:which(Module))}));
                 {ok, _} ->
                     unmodified;
                 {error, enoent} ->
                     %% The Erlang compiler deletes existing .beam files if
                     %% recompiling fails.  Maybe it's worth spitting out a
                     %% warning here, but I'd want to limit it to just once.
                     gone;
                 {error, Reason} ->
                     io:format("Error reading ~s's file info: ~p~n",
                               [Filename, Reason]),
                     error
             end
     end || {Module, _} <- code:all_loaded()].

%% @spec recompile(Module, Filename, Options) -> any
%% @doc Compiles a specified Filename with Options
recompile(Module, Filename, Options) ->
    io:format("Recompiling ~p ...", [Module]),
    case compile:file(Filename, [return | Options]) of 
        {ok, Module} ->
            io:format(" ok.~n");
        {ok, Module, []} ->
            io:format(" ok.~n");
        {ok, Module, Warnings} ->
            io:format(" ok, but with following warnings:~n~p~n", [Warnings]);
        {error, Errors, Warnings} ->
            io:format(" error:~n~p~n~p~n", [Errors, Warnings]);
        error ->
            io:format(" error.~n")
    end.


%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
