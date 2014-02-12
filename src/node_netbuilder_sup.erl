%% @doc netbuilder supervisor
%% @author Group 9

-module(node_netbuilder_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%%%------------------------------------------------------------
%%% Supervisor start_link
%%%------------------------------------------------------------

%% @spec start_link() -> ok 
%% @doc The function called to start the supervisor
start_link() ->
	supervisor:start_link( {local, ?MODULE}, ?MODULE, []).
	
%%%------------------------------------------------------------
%%% Supervisorns callback function
%%%------------------------------------------------------------

%% @spec init(Args) -> {ok, State}
%% @doc The function called when the node_netbuilder_sup is activated.
init([]) ->
	io:format("node_netbuilder_sup started, pid: ~p ~n",[self()]),
	
	RestartStrategy = {one_for_one, 1000, 3600},

	NetbuilderServSpecs = [ {node_netbuilder_serv,
					{node_netbuilder_serv, start_link, []},
					transient,
					1000,
					worker,
					[node_netbuilder_serv]}],
	
	{ok,{RestartStrategy, NetbuilderServSpecs}}.

%%%------------------------------------------------------------
%%% Public functions
%%%------------------------------------------------------------
