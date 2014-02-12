%% @doc Application module for Yaba.
%% @author Group 9
-module(node_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @spec start(Type,Args) -> term()
%% @doc Starts the supervisor tree. Called by application:start(yaba)
start(_Type, _Args) ->
	node_sup:start_link().

%% @spec stop(State) -> ok
%% @doc Stops the supervisor tree. Called by application:stop(yaba)
stop(_State) ->
	ok.