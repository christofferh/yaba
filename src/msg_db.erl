%% @author Group 9

%% @doc Ets database module for saving messages in the application
-module(msg_db).
-include("../include/msg_db.hrl").
-export([create_table/0, insert/1, delete_topic/1, delete_msg/1, drop_table/0]).
-export([msg_by_id/1, msgs_by_topic/1, print/0, info/0]).
-include_lib("eunit/include/eunit.hrl").


%% @doc Creates msg_db ets table.
%% @spec create_table() -> term()
create_table() ->
	ets:new(msg_db,[set, named_table, public, {keypos, #msg_db.id}]).


%% @doc Inserts message Msg to msg_db.
%% @spec insert(Record) -> term()
insert(#msg_db{id = Id, topic = Topic, timestamp = _Time, data = _Data} = Msg) 
	when is_atom(Topic), is_integer(Id) ->
	ets:insert(msg_db, Msg),
	ok.


%% @doc Returns information of the msg_db ets table.
%% @spec info() -> term()
info() ->
	ets:info(msg_db).


%% @doc Remove message with matching topic.
%% @spec delete_topic(Record) -> term()
delete_topic(#msg_db{topic = Topic}) when is_atom(Topic) ->
	ets:select_delete(msg_db, [{ #msg_db{	topic=Topic,  
	                      					_ = '_'},
	                						[],
	                						[true]}]),
	ok.
	

%% @doc	Remove messages with matching id.
%% @spec delete_msg(msg_db()) -> ok
delete_msg(#msg_db{id = Id}) when is_integer(Id) ->
	ets:select_delete(msg_db, [{ #msg_db{	id=Id,  
	                      					_ = '_'},
	                						[],
	                						[true]}]),
	ok.


%% @doc Deletes the entire msg_db ets table.
%% @spec drop_table() -> term()
drop_table() ->
	ets:delete(msg_db),
	ok.
	

%% @doc Returns a list of messages with the id Id.
%% @spec msg_by_id(msg_db()) -> [msg_db()]
msg_by_id(#msg_db{id = Id}) when is_integer(Id) ->
	ets:select(msg_db, [{ #msg_db{	id=Id,  
	                      			_ = '_'},
	                				[],
	                				['$_']}]).


%% @doc Returns a list all msgs which listens to the topic Topic.
%% @spec msgs_by_topic(Record) -> Msg_List
msgs_by_topic(#msg_db{topic = Topic}) when is_atom(Topic) ->
	ets:select(msg_db, [{ #msg_db{	topic = Topic,  
	                      			_ = '_'},
	                				[],
	                				['$_']}]).


%% @doc Prints out the msg_db ets table.
%% @spec print()-> term()
print() ->
	ets:tab2list(msg_db).

%% @hidden
eunit_test_() ->	
	[?_assertMatch(msg_db, create_table()),
	?_assertMatch(ok, insert(#msg_db{id = 1, topic = car, timestamp = 1337})),
	?_assertMatch(ok, insert(#msg_db{id = 2, topic = car, timestamp = 1338})),
	?_assertMatch(ok, insert(#msg_db{id = 3, topic = bike, timestamp = 1339})),
	?_assertMatch(ok, insert(#msg_db{id = 4, topic = train, timestamp = 1340}))].

%% @hidden
delete_test_() ->
	[?_assertMatch(ok, delete_topic(#msg_db{topic = bike})),
	?_assertMatch(ok, delete_msg(#msg_db{id = 4})),
	?_assertMatch(ok, delete_topic(#msg_db{topic = car})),
	?_assertMatch(ok, insert(#msg_db{id = 1, topic = car, timestamp = 1337})),
	?_assertMatch(ok, insert(#msg_db{id = 2, topic = car, timestamp = 1338})),
	?_assertMatch(ok, insert(#msg_db{id = 4, topic = train, timestamp = 1340}))].

%% @hidden
msg_test_() ->
	[?_assertEqual([{msg_db, 1, car, 1337, undefined}, {msg_db, 2, car, 1338, undefined}], msgs_by_topic(#msg_db{topic = car})),
	?_assertEqual([{msg_db, 4, train, 1340, undefined}], msg_by_id(#msg_db{id = 4}))].

%% @hidden
drop_table_test_() ->
	[?_assertMatch(ok, drop_table()),
	?_assertMatch(undefined, ets:info(msg_db))].

%% @type msg_db() = #msg{id = int(), topic = atom(), timestamp = {int(),int(),int()}, data = binary()}.
%% 		A message in Yabadanet.