%% @doc Database interface for node / topic database
%% @author Group 9

-module(node_db).
-include("../include/node_db.hrl").
-export([create_table/0, insert/1, hostname_by_topic/1, topics_by_hostname/1, exclude_hostname/1]).
-export([delete_topic/1, delete_hostname/1, drop_table/0, print/0]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Creates pid_db ets table
%% @spec create_table() -> pid_db
create_table() ->
	ets:new(pid_db,[bag, named_table, public, {keypos, #node.topic}]),
	ets:new(pid_db_rev,[bag, named_table, public, {keypos, #node.hostname}]),
	ok.


%% @doc Inserts node to pid_db
%% @spec insert(Record) -> term()
insert(#node{topic=Topic} = Node) when is_atom(Topic) ->
	ets:insert(pid_db, Node),
	ets:insert(pid_db_rev, Node),
	ok.
	
	
%% @doc Returns a list of all pids which listens to the Topic
%% @spec hostname_by_topic(Record) -> Pid_List
hostname_by_topic(#node{topic = Topic}) when is_atom(Topic) ->
	ets:select(pid_db, [{ #node{	topic = Topic,  
	                      			hostname = '$1'},
	                				[],
	                				['$1']}]).


%% @doc Returns a list of topics which Pid listens to
%% @spec topics_by_hostname(Record) -> Topic_List
topics_by_hostname(#node{hostname=Hostname}) ->
		ets:select(pid_db_rev, [{ #node{hostname = Hostname,  
		                      			topic = '$1'},
		                				[],
		                				['$1']}]).

	
%% @doc Deletes the reference of Pid from Topic
%% @spec exclude_hostname(Record) -> term()
exclude_hostname(#node{topic=Topic} = Node) when is_atom(Topic) ->
	ets:delete_object(pid_db, Node),
	ets:delete_object(pid_db_rev, Node),
	ok.


%% @doc Deletes the Topic
%% @spec delete_topic(Record) -> term()
delete_topic(#node{topic=Topic}) when is_atom(Topic) ->
	ets:select_delete(pid_db, [{ #node{topic=Topic,  
	                      					_ = '_'},
	                						[],
	                						[true]}]),
	ets:select_delete(pid_db_rev, [{ #node{topic=Topic,  
											_ = '_'},
											[],
											[true]}]),
	ok.
	
	
%% @doc Deletes all references to Pid
%% @spec delete_hostname(Record) -> term()
delete_hostname(#node{hostname=Hostname}) ->
	ets:select_delete(pid_db, [{ #node{		hostname=Hostname,  
	                      					_ = '_'},
	                						[],
	                						[true]}]),
	ets:select_delete(pid_db_rev, [{ #node{	hostname=Hostname,  
	  										_ = '_'},
	                						[],
											[true]}]),	
	ok.

%% @doc Deletes the ets table
%% @spec drop_table() -> term()
drop_table() ->
	ets:delete(pid_db),
	ets:delete(pid_db_rev),
	ok.

%% @doc Prints out pid_db
%% @spec print() -> Node_List
print() ->
	ets:tab2list(pid_db). 


%% @hidden
insert_test_() ->	
	[?_assertMatch(ok, create_table()),
	?_assertMatch(ok, insert(#node{topic = car, hostname = "node1"})),
	?_assertMatch(ok, insert(#node{topic = car, hostname = "node2"})),
	?_assertMatch(ok, insert(#node{topic = bike, hostname = "node3"})),
	?_assertMatch(ok, insert(#node{topic = truck, hostname = "node4"})),
	?_assertMatch(ok, insert(#node{topic = truck, hostname = "node5"})),
	?_assertMatch(ok, insert(#node{topic = truck, hostname = "node6"}))].

%% @hidden
hostname_by_topic_test_() ->
	[?_assertEqual(["node3"], hostname_by_topic(#node{topic = bike})),
	?_assertMatch([], hostname_by_topic(#node{topic = train})),
	?_assertEqual(["node1", "node2"], hostname_by_topic(#node{topic = car}))].

%% @hidden
topics_by_hostname_test_() ->
	[?_assertMatch([bike], topics_by_hostname(#node{hostname = "node3"})),
	?_assertMatch([car], topics_by_hostname(#node{hostname = "node1"})),
	?_assertMatch([], topics_by_hostname(#node{hostname = "node9"})),
	?_assertMatch([truck], topics_by_hostname(#node{hostname = "node6"}))].

%% @hidden	
exclude_hostname_test_() ->
	[?_assertEqual(["node3"], hostname_by_topic(#node{topic = bike})),
	?_assertMatch(ok, exclude_hostname(#node{topic = bike, hostname = "node3"})),
	?_assertMatch([], hostname_by_topic(#node{topic = bike})),
	?_assertMatch(ok, exclude_hostname(#node{topic = bike, hostname = "node3"}))].

%% @hidden	
delete_topic_test_() ->
	[?_assertMatch(ok, delete_topic(#node{topic = car})),
	?_assertMatch([], hostname_by_topic(#node{topic = car}))].

%% @hidden	
delete_hostname_test_() ->
	[?_assertMatch(ok, delete_hostname(#node{hostname = "node4"})),
	?_assertMatch(ok, delete_hostname(#node{hostname = "node5"})),
	?_assertEqual(["node6"], hostname_by_topic(#node{topic = truck}))].

%% @hidden	
drop_table_test_() ->
	[?_assertMatch(ok, drop_table()),
	?_assertMatch(undefined, ets:info(pid_db))].

%% @type msg() = #msg{id = int(), topic = atom(), timestamp = {int(),int(),int()}, data = binary()}.
%% 		A message in Yabadanet.