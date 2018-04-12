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
    NewState = maps:put(Key, Value, State),
    {reply, ok, NewState};
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calback helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
store()
