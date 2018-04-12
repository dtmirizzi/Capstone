%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Database Node
%% By DT Mirizzi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(dtdb).

-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API Exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([get_state/0, delete/1, find/1, store/2]).

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
    case gen_server:start_link({local, dtdb}, dtdb, [], [])
	of
      {ok, Pid} -> global:register_name(node(), Pid);
      {error, _} -> io:write("1")
    end.

store(Key, Value) ->
    gen_server:call(dtdb, {store, Key, Value}).

find(Key) -> gen_server:call(dtdb, {find, Key}).

delete(Key) -> gen_server:call(dtdb, {delete, Key}).

get_state() -> gen_server:call(dtdb, {get_state}).

stop() -> gen_server:stop(dtdb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callback src
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) ->
    Empty_Map = maps:new(),
    io:write(Empty_Map),
    {ok, Empty_Map}.

handle_call({store, Key, Value}, _From, State) ->
    NewState = maps:put(Key, Value, State),
    {reply, ok, NewState};

handle_call({get_state}, _From, State) ->
    {reply, ok, State};

handle_call({find, Key}, _From, State) ->
    Value = try {ok, [V]} = maps:find(Key, State), V catch
	      error -> false
	    end,
    {reply, Value, State};
handle_call({delete, Key}, _From, State) ->
    NewState = maps:remove(Key, State),
    {reply, ok, NewState}.

handle_cast(_Request, State) -> {reply, ok, State}.

terminate(_Reason, _State) ->
    %remove node from gobal list
    global:unregister_name(node()).
