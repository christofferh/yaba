%% @doc netbuilder gen_server
%% @author Group 9

-module(node_netbuilder_serv).
-behaviour(gen_server).
-export([start_link/0, write/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, send_discovery_msg/0, crash/0]).
-record(state, {send, recv, timer, tref}).
%-record(msg, {msg, type}).
-define(PORT, 40000).
-define(MULTICAST_ADDR, {224, 0, 0, 100}).
-define(LOCAL_ADDR, {0,0,0,0}).
-import(node_db).
-include("../include/node_db.hrl").

%%%------------------------------------------------------------
%%% Gen_server start_link
%%%------------------------------------------------------------

%% @doc The function called by the supervisor(node_netbuilder_sup),
%% to start the gen_server and link it to its supervisor.
%% @spec start_link() -> ok 
start_link() ->
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [], []).
	
%%%------------------------------------------------------------
%%% Gen_server callback functions
%%%------------------------------------------------------------


%% @doc The function called when the gen_server is activated.
%% @spec init(Args) -> {ok, State}
init([]) ->
	io:format("node_netbuilder_serv started, pid: ~p ~n",[self()]),
	SendSocket = make_send_socket(),
    RecvSocket = make_recv_socket(),
	net_kernel:monitor_nodes(true, []),
	timer:start(),
	case timer:send_interval(10000, {discovery}) of
		{ok, TRef} ->
			{ok, #state{send=SendSocket, recv=RecvSocket, timer = true, tref = TRef}};
		{error, Reason} ->
			io:format("Unable to start timer interval.~nAutomatic node discovery won´t work.~n reason: ~p ~n",[Reason]),
			{ok, #state{send=SendSocket, recv=RecvSocket, timer = false}}
	end.

%% @doc The callback function when the gen_server is called with
%% gen_server:call/2, returns Return_value to the caller.
%% @spec handle_call(Request, From, State) -> {reply, Return_value, State}
handle_call({crash}, _From, State) ->
	X = 1,
	{reply, X = 2, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% @doc The callback function when the gen_server is called with
%% gen_server:cast/2, does not return a value.
%% @spec handle_cast(Request, State) -> {noreply, State}
handle_cast({send_msg}, State) ->
	MSG = create_msg(),
	send_msg(MSG, State),
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

%% @doc The callback function when the gen_server is either timed out
%% or recieves a message from varius sources.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info({udp, _Socket, _IP, _InPortNo, Packet}, #state{tref = TRef} = State) ->
	My_Hostname = node(),
	Hostname = binary_to_term(Packet),
	case Hostname == My_Hostname of
		true ->
			{noreply, State};
		false ->
			cancel_timer(TRef),
			My_Topics = node_db:topics_by_hostname(#node{hostname = My_Hostname}),
			{node_netbuilder_serv, Hostname} ! {subscribing_nodes,{My_Hostname, My_Topics}},
			io:format("Received discovery message~n"),
			NewState = State#state{timer = false, tref = undefined},
			{noreply, NewState}
	end;

handle_info({subscribing_nodes,{Node_Hostname, Node_Topics}}, State) ->
	cancel_timer(State#state.tref),
	My_Hostname = node(),
	My_Topics = node_db:topics_by_hostname(#node{hostname = My_Hostname}),
	{node_netbuilder_serv, Node_Hostname} ! {subscribing_nodes_halt,{My_Hostname, My_Topics}, []},
	lists:foreach(fun(Topic) ->
	node_db:insert(#node{hostname = Node_Hostname, topic = Topic})
	end, Node_Topics),
	{noreply, State};
handle_info({subscribing_nodes_halt,{Node_Hostname, Node_Topics}, _}, State) ->
	lists:foreach(fun(Topic) ->
	node_db:insert(#node{hostname = Node_Hostname, topic = Topic})
	end, Node_Topics),
	{noreply, State};
handle_info({nodedown, Hostname}, State) ->
	io:format("Node: ~p is down ~n",[Hostname]),
	node_db:delete_hostname(#node{hostname = Hostname}),
	case erlang:nodes() =:= [] of
		true ->
			case timer:send_interval(30000, {discovery}) of
				{ok, TRef} ->
					{noreply, State#state{timer = true, tref = TRef}};
				{error, Reason} ->
					io:format("Unable to start timer interval.~nAutomatic node discovery won´t work.~n reason: ~p ~n",[Reason]),
					{noreply, State#state{timer = false}}
			end;
		false ->
			{noreply, State}
	end;
handle_info({nodeup, Node}, State) ->
	io:format("Node: ~p is up ~n",[Node]),
	{noreply, State};
handle_info({discovery}, State) ->
	MSG = create_msg(),
	send_msg(MSG, State),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @doc The callback function when the gen_server is terminated.
%% @spec terminate(Reason, State) -> term()
terminate(Reason, _State) ->
	io:format("node_netbuilder_serv terminates, reason: ~p ~n",[Reason]),
	ok.

%% @doc The callback function when the gen_server should upgrade/downgrade
%% it´s internal state or load new modules
%% @spec code_change(oldVsn, State, Extra) -> {ok, Newstate}
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%%%------------------------------------------------------------
%%% Gen_server states
%%%------------------------------------------------------------

%% @doc	Send
%% @spec send_msg(Packet, Record) -> term()
send_msg(Packet, #state{send=Send}=_State) ->
	case gen_udp:send(Send, ?MULTICAST_ADDR, ?PORT, Packet) of
        ok ->
			io:format("Sending discovery message~n",[]),
            ok;
        {error, Reason} ->
            io:format("~p Unable to send packet (~p)~n", [node(), Reason])
    end.
	
%%%------------------------------------------------------------
%%% Gen_server interface
%%%------------------------------------------------------------

%% @doc Does nothing
%% @spec write() -> term()
write() ->
	gen_server:call(?MODULE, []).

%% @doc Sends discovery msg
%% @spec send_discovery_msg() -> term()
send_discovery_msg() ->
	gen_server:cast(?MODULE, {send_msg}).

%%%------------------------------------------------------------
%%% private functions
%%%------------------------------------------------------------
	
%% @doc Cancels the interval timer that sends udp messages
%% @spec cancel_timer(TRef) -> ok
cancel_timer(TRef) ->
	case timer:cancel(TRef) of
		{ok, cancel} ->
			ok;
		{error, _Reason} ->
			ok
	end.

%% @spec create_msg() -> term_to_binary(Msg_list)
%% @doc Converts the message tuple into a binary message
create_msg() ->
	Msg_list = node(),
	term_to_binary(Msg_list).
	
%% @spec make_recv_socket()  -> {ok,State}
%% @doc Creates a receive socket
make_recv_socket() ->
    Opts = [ { active, true },
             { ip, ?MULTICAST_ADDR },
             { add_membership, { ?MULTICAST_ADDR, ?LOCAL_ADDR } },
             { multicast_loop, true },
             { reuseaddr, true },
             binary ],

    { ok, Socket } = gen_udp:open (?PORT, Opts),
    Socket.

%% @spec make_send_socket() -> {ok,State}
%% @doc Creates a sending socket
make_send_socket() ->
    Options = [ { ip, ?LOCAL_ADDR },
                { multicast_ttl, 255 }, 
                { multicast_loop, true } ],
    {ok, Socket} = gen_udp:open(0, Options),
    Socket.

%% @spec crash() -> term()
%% @doc Crashes the genserver.
crash() ->
	io:format("Netbuilder Serv crashing !",[]),
	gen_server:call(?MODULE, {crash}).
