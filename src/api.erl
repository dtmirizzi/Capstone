%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Master node
%% By DT Mirizzi
%% No State is currently being saved in this module but may be used later
%% to store api keys or can be deprecated/ignored
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(api).

-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([delete/1, find/1, store/2]).

-export([start_link/0, stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callback Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([handle_call/3, handle_cast/2, init/1,
	 terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API src
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, api}, api, [], [])

store(Key, Value) ->
    gen_server:call(api, {store, Key, Value}).

find(Key) -> gen_server:call(api, {find, Key}).

delete(Key) -> gen_server:call(api, {delete, Key}).

stop() -> gen_server:stop(api).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callback src
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Args) -> Empty_Map = maps:new(), {ok, Empty_Map}.

handle_call({store, Key, Value}, _From, State) ->
	Pid = spawn(?MODULE, storeInternal, [Key, Value]),
	{reply, ok, State}.
handle_call({find, Key}, _From, State) ->
    {_, Results} = findInternal(Key),
    {reply, Results, State};
handle_call({delete, Key}, _From, State) ->
	Pid = spawn(?MODULE, deleteInternal, [Key]),
	{reply, ok, State}.

handle_cast(_Request, State) -> {reply, error, State}.

terminate(_Reason, _State) ->
	{ok}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calback helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
storeInternal(Key, Value)->
	storeInternal(global:registered_names(), Key, Value, length(global:registered_names())).
storeInternal([], _K, _V, _GraphSz)->
	{ok};
storeInternal([Node|Nodes], Key, Value, GraphSz)->
	if
		GraphSz<=2 ->
			gen_server:cast({gobal, Node}, {store, Key, Value}),
			storeInternal(Nodes, Key, Value, GraphSz -1)
	end,
	if
		 rem 2 == 0 ->
			gen_server:cast({global,Node}, {delete, Key, Value}),
			storeInternal(Nodes, Key, Value, GraphSz -1)
	end.

deleteInternal(Key)->
	storeInternal(global:registered_names(), Key).
deleteInternal([], _K)->
	{ok};
deleteInternal([Node|Nodes], Key)->
	gen_server:cast({global, Node}, {delete, Key}).

findInternal(Key)->
	storeInternal(global:registered_names(), Key, []).
findInternal([], _K, Return)->
	{ok, Return};
findInternal([Node|Nodes], Key, Return)->
	{_, Reply} = gen_server:call({global, Node}, {delete, Key}),
	findInternal(Nodes, Key, lists:append(Return, Reply)).
