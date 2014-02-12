%% @doc node supervisor
%% @author Group 9

-module(node_sup).
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
%% @doc The function called when the node__sup is activated.
init([]) ->
	io:format("node_sup started, pid: ~p ~n",[self()]),
	node_db:create_table(),
	msg_db:create_table(),
	RestartStrategy    = one_for_one,
	MaxRestarts        = 1000,
	MaxTimeBetRestarts = 3600,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

	NetbuilderSupSpecs = {node_netbuilder_sup,
					{node_netbuilder_sup, start_link, []},
					permanent,
					1000,
					supervisor,
					[node_netbuilder_sup]},
					
	IpcServSpecs = {node_ipc_serv,
					{node_ipc_serv, start_link, []},
					transient,
					1000,
					worker,
					[node_ipc_serv]},
					
	DistServSpecs = {node_dist_serv,
					{node_dist_serv, start_link, []},
					transient,
					1000,
					worker,
					[node_dist_serv]},
	
	{ok,{SupFlags, [NetbuilderSupSpecs, IpcServSpecs, DistServSpecs] }}.
