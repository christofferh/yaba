%% @doc ipcelr gen_server
%% @author Group 9

-module(node_ipc_serv).
-behaviour(gen_server).
-export([start_link/0, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, notify_clients/1]).
% -record(servstate, {send, recv}).
-include("../include/msg_db.hrl").

%%%------------------------------------------------------------
%%% Gen_server start_link
%%%------------------------------------------------------------

%% @doc The function called by the supervisor(node_sup),
%% to start the gen_server and link it to it´s supervisor.
%% @spec start_link() -> ok 
start_link() ->
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [], []).
	
%%%------------------------------------------------------------
%%% Gen_server callback functions
%%%------------------------------------------------------------

%% @spec init(args) -> {ok,State} 
%% @doc The function called when the gen_server is activated
%% Should return a state in form of a function, in this 
%% case the call to loop_func().
init([]) ->
	io:format("node_ipc_serv started, pid: ~p ~n",[self()]),
	{ok, []}.

%% @spec handle_call(Request, From, State) -> {reply, Return_value, State}
%% @doc The function called by the gen_server when the server
%% is called with gen_server:call().
handle_call(_Request, _From, _State) ->
	{reply, ok, []}.

%% @spec handle_cast(Request, State) -> {noreply, State}
%% @doc The function called by the gen_server is called with gen_server:cast()
handle_cast({notify, #msg_db{topic = Topic, id = Id, timestamp = Timestamp, data = Data} = _Record}, State) ->
	{mbox, 'jaba@localhost'} ! {message, {Id, Topic, Timestamp, Data}},
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

%% @spec handle_info(Info, State) -> {noreply, State}
%% @doc Is called when the gen_server is timed out(?).
handle_info({subscribe, Topic}, State) ->
	node_dist_serv:subscribe_topic(Topic),
	{noreply, State};
handle_info({unsubscribe,Topic}, State) ->
	node_dist_serv:unsubscribe_topic(Topic),
	{noreply, State};
handle_info({publish,MSG}, State) ->
	node_dist_serv:publish_message(MSG),
	{noreply, State};
handle_info(_Info, State) ->
	io:format("Received info message.~n"),
	{noreply, State}.


%% @doc The function called by the gen_server when the server
%% is about to terminate, either by choice or accident.
%% Use to clean up 'everything' before termination.
%% @spec terminate(Reason, State) -> term()
terminate(_Reason, _State) ->
	io:format("Goodbye...",[]),
	ok.

%% @spec code_change(oldVsn, State, Extra) -> {ok, Newstate}
%% @doc The callback function when the gen_server should upgrade/downgrade
%% it´s internal state or load new modules.	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%%%------------------------------------------------------------
%%% Gen_server private functions
%%%------------------------------------------------------------


%%%------------------------------------------------------------
%%% Gen_server interface
%%%------------------------------------------------------------


%% @spec notify_clients(MSG) -> ok
%% @doc	Notifies the local client of a new incoming message.
notify_clients(MSG) ->
	gen_server:cast(?MODULE, {notify, MSG}).

