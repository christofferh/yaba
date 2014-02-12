%% @doc Dist gen_server
%% @author Group 9

-module(node_dist_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, crash/0]).
-export([publish_message/1, subscribe_topic/1, unsubscribe_topic/1]).
-import(node_db).
-import(msg_db).
-include("../include/node_db.hrl").
-include("../include/msg_db.hrl").

%%%------------------------------------------------------------
%%% Gen_server start_link
%%%------------------------------------------------------------

%% @spec start_link() -> term()
%% @doc The function called by the supervisor
start_link() ->
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [], []).
	
%%%------------------------------------------------------------
%%% Gen_server callback functions
%%%------------------------------------------------------------



%% @spec init(Args) -> term()
%% @doc The function called when the gen_server is activated
%% Should return a state in form of a function, in this 
%% case the call to loop_func().
init([]) ->
	io:format("node_dist_serv started, pid: ~p ~n",[self()]),
	{ok, []}.


%% @spec handle_call(Request, From, State) -> {reply, term(), State}
%% @doc The function called by the gen_server when the server.
%% is called with gen_server:call().
handle_call({crash}, _From, State) ->
	X = 1,
	{reply, X = 2, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.


%% @spec handle_cast(Request, State) -> {noreply, State}
%% @doc The function called by the gen_server when the server.
%% is called with gen_server:cast().
handle_cast({subscribe_topic, Record}, State) ->
	node_db:insert(Record),
	lists:foreach(fun(Hostname) ->
	{node_dist_serv, Hostname} ! {new_topic, Record}
	end, erlang:nodes()),
	{noreply, State};
handle_cast({unsubscribe_topic, Record}, State) ->
	node_db:exclude_hostname(Record),
	lists:foreach(fun(Hostname) ->
	{node_dist_serv, Hostname} ! {remove_topic, Record}
	end, erlang:nodes()),
	{noreply, State};
handle_cast({publish_message, #msg_db{topic = Topic} = Record}, State) ->
	io:format("Publishing message~n",[]),
	% msg_db:insert(Record), 
	lists:foreach(fun(Hostname) ->
		{node_dist_serv, Hostname} ! {message, Record}
		end, node_db:hostname_by_topic(#node{topic=Topic})),
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.


%% @spec handle_info(Info, State) -> {noreply, State}
%% @doc Is called when the gen_server is timed out(?)
handle_info({message, #msg_db{topic = Topic, id = Id, timestamp = Timestamp, data = Data} = Record}, State) ->
	io:format("Received message ~p~n", [Id]),
	io:format("           Topic ~p~n", [Topic]),
	io:format("       Timestamp ~p~n", [Timestamp]),
	io:format("            Data ~p~n", [Data]),
	
	%% Check if received message should be stored or not
	case msg_db:msg_by_id(#msg_db{id = Id}) of
		[] ->
			msg_db:insert(Record),
			node_ipc_serv:notify_clients(Record);
		[#msg_db{timestamp = DbTimestamp}] ->
			case (0 > timer:now_diff(DbTimestamp, Timestamp)) of
					true ->
						msg_db:insert(Record),
						node_ipc_serv:notify_clients(Record);
					false ->
						ok
			end
	end,
	{noreply, State};
handle_info({new_topic, Record}, State) ->
	node_db:insert(Record),
	{noreply, State};
handle_info({remove_topic, Record}, State) ->
	node_db:exclude_hostname(Record),
	{noreply, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @spec terminate(Reason, State) -> term()
%% @doc The function called by the gen_server when the server
%% is about to terminate, etiher by choice or accident.
%% Use to clean up 'everything' before termination.
terminate(_Reason, _State) ->
	io:format("Goodbye...",[]),
	ok.



%% @spec code_change(OldVsn, State, Extra) -> {ok, State}
%% @doc Code changed or state
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%%%------------------------------------------------------------
%%% Gen_server private functions
%%%------------------------------------------------------------

	
%%%------------------------------------------------------------
%%% Gen_server interface
%%%------------------------------------------------------------


% Test publishing with these lines
% node_sup:start_link().
% node_dist_serv:publish_message({taxi, 1, {"Sven", 100, 100}}).


%% @spec subscribe_topic(Topic) -> ok
%% @doc Distrubutes a new topic to all connected nodes
subscribe_topic(Topic) when is_atom(Topic) ->
	gen_server:cast(?MODULE, {subscribe_topic, #node{topic = Topic, hostname = node()}}).
	

%% @spec unsubscribe_topic(Topic) -> ok
%% @doc Sends a unsubscribe message to all connected nodes
unsubscribe_topic(Topic) when is_atom(Topic) ->
	gen_server:cast(?MODULE, {unsubscribe_topic, #node{topic = Topic, hostname = node()}}).

%% @spec publish_message(Msg) -> term()
%% @doc Publish a message
publish_message({Topic,Id,Data}) ->
	Timestamp = erlang:now(),
	gen_server:cast(?MODULE, {publish_message, #msg_db{id = Id, topic = Topic, timestamp = Timestamp, data = Data}}).

%% @spec crash() -> term()	
%% @doc Crashes the genserver.
crash() ->
	io:format("Dist Serv crashing !",[]),
	gen_server:call(?MODULE, {crash}).

%% @type msg() = #msg{id = int(), topic = atom(), timestamp = {int(),int(),int()}, data = binary()}.
%% 		A message in Yabadanet.