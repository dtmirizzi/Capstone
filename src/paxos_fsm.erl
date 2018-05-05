%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Paxos State Machine
%% By DT Mirizzi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(paxos_fsm).

-behaviour(gen_fsm).

-export([start/5, stop/1, get_result/1]).

-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([nil/2, preparing/2, proposing/2, acceptor/2,
         learner/2, decided/2]).

-define( DEFAULT_TIMEOUT, 3000 ).

-record( state, {subject, n, value,
                 all, quorum, current=0, others, init_n,
                 return_pids=[]
                } ).

start(S, InitN, V, Others, ReturnPids) ->
    All = length(Others)+1,    Quorum = All / 2 ,
    InitStateData = #state{ subject=S, n=InitN, value=V,
                            all=All, quorum=Quorum, others=Others, init_n=InitN,
                            return_pids=ReturnPids },
    gen_fsm:start_link(
      generate_global_address( node(), S ),
      ?MODULE,
      InitStateData,
      [{timeout, ?DEFAULT_TIMEOUT}]
     ).

stop(S) ->
    gen_fsm:send_all_state_event( generate_global_address( node(),S ), stop).

get_result(S)->
    gen_fsm:sync_send_all_state_event( generate_global_address( node(),S ), result).

init(InitStateData)->
    io:format("~p ~p: ~p~n", [?MODULE, started, InitStateData]),
    process_flag(trap_exit, true),
    {ok,
     nil,
     InitStateData,
     ?DEFAULT_TIMEOUT
    }.

broadcast(Others, S, Message)->
    PaxosOthers = [ generate_global_address( P, S ) || P <-  Others ],
    lists:map( fun(Other)-> gen_fsm:send_event( Other, Message ) end ,
               PaxosOthers ).

send(Node, S, Message)->
    gen_fsm:send_event( generate_global_address( Node, S ), Message ).

get_next_n( N , All )-> (( N div All )+1) * All.

generate_global_address( Node, Subject )->  {global, {?MODULE, Node, Subject}}.

%% =========================================
%% states:
%%  - nil
%%  - preparing
%%  - proposing
%%  - acceptor
%%  - learner
%%  - decided
%% events: n' < n < n''...
%%  - {prepare, {S, N, V}}
%%  - {prepare_result, {S, N, V}}
%%  - propose
%%  - propose_result
%%  - timeout
%%  - decide
%% Data:
%%  - {{Sc, Nc, Vc}, {All, Quorum, Current, Others, InitN}}

nil( {prepare,  {S, N, _V, From}},  StateData) when N > StateData#state.n ->
    send(From, S, {prepare_result, {S, 0, nil, node()}}),
    NewStateData = StateData#state{n=N},
    {next_state, acceptor, NewStateData,
     ?DEFAULT_TIMEOUT};
nil( {prepare,  {S, N, _V, From}}, StateData) when N < StateData#state.n ->
s    send(From, S, {prepare_result, {S, StateData#state.n, StateData#state.value, node()}}),
    {next_state, nil, StateData, ?DEFAULT_TIMEOUT};

nil( {decide,  {S, N, V, _From}}, StateData ) ->
    S=StateData#state.subject,
    decided_callback( StateData#state{n=N, value=V} );

nil( timeout, StateData )->
    NewN = get_next_n( StateData#state.n, StateData#state.all ) + StateData#state.init_n,
    io:format( "starting paxos ... ~p. ~n", [[NewN, StateData]]),
    S=StateData#state.subject,
    V=StateData#state.value,
    Result = broadcast( StateData#state.others, S, {prepare, {S, NewN, V, node()}} ),
    io:format( "broadcast: ~p. ~n", [Result]),
    {next_state, preparing, StateData#state{n=NewN, current=1}, ?DEFAULT_TIMEOUT};

nil(UnknownEvent, StateData)-> % ignore
    io:format( "unknown event: ~p,  ~p : all ignored.~n", [UnknownEvent, StateData] ),
    {next_state, nil, StateData, ?DEFAULT_TIMEOUT}.

preparing( {prepare,  {S, N, _V, From}},  StateData ) when N < StateData#state.n ->
    send( From, S,  {prepare, {S, StateData#state.n, StateData#state.value, node()} } ),
    {next_state, preparing, StateData, ?DEFAULT_TIMEOUT};
preparing( {prepare,  {S, N, V, From}},  StateData ) when N > StateData#state.n ->
    send( From, S, {prepare_result, {S, StateData#state.n, StateData#state.value, node()}}),
    io:format("sending prepare_result and going acceptor...~n", []),
    {next_state, acceptor, StateData#state{subject=S, n=N, value=V}, ?DEFAULT_TIMEOUT};

preparing( {prepare_result,  {S, N, V, From}},  StateData ) when N > StateData#state.n ->
    send( From, S, {prepare_result, {S, N, V, node()}} ),
    {next_state, acceptor, StateData#state{subject=S, n=N, value=V}, ?DEFAULT_TIMEOUT};

preparing( {prepare_result,  {S, _N, _V, _From}}, StateData ) when StateData#state.current > StateData#state.quorum ->
    broadcast( StateData#state.others, S, {propose, {S,StateData#state.n,StateData#state.value, node()}} ),
    {next_state, proposing, StateData#state{current=1}, ?DEFAULT_TIMEOUT};

preparing( {prepare_result,  {S, N, V, _From}}, StateData )
  when S==StateData#state.subject , N==StateData#state.n , V==StateData#state.value ->
    Current = StateData#state.current,
    {next_state, proposing, StateData#state{current=Current+1}, ?DEFAULT_TIMEOUT};

preparing( {prepare_result,  {S, N, _V, _From}}, StateData ) when N < StateData#state.n ->
    case (StateData#state.current + 1 > StateData#state.quorum) of
        true ->
            io:format("got quorum at prepare!~n", []),
            broadcast( StateData#state.others, S, {propose, {S, StateData#state.n, StateData#state.value, node()}} ),
            {next_state, proposing, StateData#state{current=1}, ?DEFAULT_TIMEOUT};
        false ->
            Current = StateData#state.current,
            {next_state, preparing, StateData#state{current=Current+1}, ?DEFAULT_TIMEOUT}

    end;

preparing( {propose,  {S, N, V, From}},  StateData ) when N > StateData#state.n ->
    send( From, S, {propose_result, {S, N, V, node()}} ),
    {next_state, learner, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

preparing( {propose_result,  {S, N, V, From}}, StateData) when N > StateData#state.n ->
    send( From, S, {propose_result, {S, N, V, node()}} ),
    {next_state, learner, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

preparing( {decide,  {_S, N, V, _From}}, StateData)->
    decided_callback( StateData#state{n=N, value=V} );

preparing( timeout, StateData)->
    {next_state, nil,  StateData#state{current=0}, ?DEFAULT_TIMEOUT}.


proposing( {prepare,  {S, N, V, From}},  StateData) when N > StateData#state.n ->
    send( From, S,  {prepare_result, {S, StateData#state.n, StateData#state.value, node() }}),
    {next_state, acceptor, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

proposing( {prepare_result,  {S, N, V, From}},  StateData) when N > StateData#state.n ->
    send( From, S, {prepare_result, {S, StateData#state.n, StateData#state.value, node()}}),
    {next_state, acceptor, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

proposing( {propose,  {S, N, V, From}},  StateData) when N > StateData#state.n ->
    send( From, S, {propose_result, {S, StateData#state.n, StateData#state.value, node()}}),
    {next_state, learner, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

proposing( {propose_result,  {S, N, V, _From}}, StateData)
  when N==StateData#state.n, V==StateData#state.value, StateData#state.quorum > StateData#state.current+1 ->
    S=StateData#state.subject,
    Current = StateData#state.current,
    {next_state, proposing, StateData#state{current=Current+1}, ?DEFAULT_TIMEOUT };

%% Got quorum!!!
proposing( {propose_result,  {S, N, V, _From}}, StateData) when N==StateData#state.n, V==StateData#state.value->
    io:format("got quorum at proposing!!~n", []),
    broadcast( StateData#state.others, S, {decide, {S, N, V, node()}} ),
    Current=StateData#state.current,
    decided_callback( StateData#state{current=Current+1} );

proposing( {propose_result,  {S, N, V, From}}, StateData) when N > StateData#state.n ->
    send( From,  S, {propose_result, {S, N, V, node()}}),
    {next_state, learner, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

proposing( {decide,  {S, N, V, _From}}, StateData) when N >= StateData#state.n->
    S=StateData#state.subject,
    decided_callback( StateData#state{n=N, value=V} );


proposing( timeout, StateData)->
    io:format("proposing timeout: ~p  , StateData=~p~n" , [propose, StateData]),
    {next_state, nil, StateData#state{current=1}, ?DEFAULT_TIMEOUT};

proposing( _Event, StateData) ->
    {next_state, proposing, StateData}.

acceptor( {prepare,  {S, N, _V, From}},  StateData) when N < StateData#state.n->
    send( From, S, { prepare_result, {S, StateData#state.n, StateData#state.value, node()}} ),
    {next_state, acceptor, StateData, ?DEFAULT_TIMEOUT};

acceptor( {prepare,  {S, N, V, From}},  StateData ) when N >= StateData#state.n ->
    send( From, S, { prepare_result, {S, StateData#state.n, StateData#state.value, node()}} ),
    {next_state, acceptor, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

acceptor( {propose,  {S, N, _V, _From}},  StateData) when N < StateData#state.n ->
    io:format("bad state: ~p (N,Nc)=(~p)~n" , [{propose},{ N, StateData#state.n}]),
    S=StateData#state.subject,
    {next_state, propose, StateData, ?DEFAULT_TIMEOUT};
acceptor( {propose,  {S, N, V, From}},  StateData ) when N > StateData#state.n ->
    send( From, S, {propose_result , {S, StateData#state.n, StateData#state.value, node() }} ),
    {next_state, learner, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};
acceptor( {propose,  {S, N, V, From}},  StateData)->
    {N,V}={StateData#state.n, StateData#state.value},
    send( From, S, {propose_result , {S, StateData#state.n, StateData#state.value,  node() }} ),
    {next_state, learner, StateData, ?DEFAULT_TIMEOUT};

acceptor( {decide,  {S, N, V, _From}}, StateData) when N >= StateData#state.n ->
    S=StateData#state.subject,
    decided_callback( StateData#state{n=N, value=V} );

acceptor( timeout, StateData)->
    io:format("acceptor timeout: ~p (N,V)=(~p)~n" , [{propose},{StateData#state.n, StateData#state.value}]),
    {next_state, nil, StateData#state{current=1}, ?DEFAULT_TIMEOUT};

acceptor( _Event, StateData) ->
    io:format("acceptor unknown event: ~p ,~p~n" , [_Event , StateData]),
    {next_state, acceptor, StateData}.


learner( {prepare,  {S, N, V, From}}, StateData) when N > StateData#state.n ->
    send( From, S, {prepare_result, {S, N, V, node()}} ),
    {next_state, acceptor, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

learner( {prepare_result,  {S, _N, _V, _From}}, StateData )->
    S=StateData#state.subject,
    {next_state, learner, StateData, ?DEFAULT_TIMEOUT };

learner( {propose,  {S, N, _V, _From}}, StateData) when N < StateData#state.n ->
    S=StateData#state.subject,
    {next_state, learner, StateData, ?DEFAULT_TIMEOUT};

learner( {propose,  {S, N, V, From}},  StateData) when N > StateData#state.n ->
    send( From, S, {propose_result, {S, N, V, node()}}),
    {next_state, learner, StateData#state{n=N, value=V}, ?DEFAULT_TIMEOUT};

learner( {decide,  {S, N, V, _From}}, StateData) when N >= StateData#state.n ->
    S=StateData#state.subject,
    decided_callback( StateData#state{n=N, value=V} );

learner( timeout, StateData)->
    {next_state, nil, StateData#state{current=0}, ?DEFAULT_TIMEOUT};

learner( _Event, StateData )->
    {next_state, learner, StateData }.

decided( {_Message, {S,_N,_V, From}}, StateData)->
    send( From, S, {decide, {S,StateData#state.n, StateData#state.value,node()}} ),
    {next_state, decided, StateData, ?DEFAULT_TIMEOUT };

decided( timeout,  StateData )->
    io:format( "paxos mediation done. decided: ~p - (N=~p)~n", [StateData#state.value, StateData#state.n] ),
%    {next_state, decided, StateData}. %{{S, N, V}, Nums}}.
    {stop, normal, StateData }.

%% @doc must be called back whenever the subject got consensus!
decided_callback(StateData)->
    callback(StateData#state.subject, StateData#state.value, StateData#state.return_pids ),
    {next_state, decided, StateData, ?DEFAULT_TIMEOUT}.

%% @doc send back message to originator to share the consensus value.
callback(S, V, ReturnPids)->
    lists:map( fun(ReturnPid)-> ReturnPid ! {self(), result, {S, V}} end, ReturnPids ).

code_change(_,_,_,_)->
    ok.

handle_event( stop, _StateName, StateData )->
    {stop, normal, StateData}.

handle_info(_,_,_)->
    ok.

handle_sync_event(result, _From, StateName, StateData)->
    {reply, {StateName, StateName#state.value}  , StateName, StateData};

handle_sync_event(stop, From, StateName, StateData)->
    {stop, From, StateName, StateData}.

terminate(Reason, StateName, StateData) ->
    io:format("~p terminated (~p).~n", [?MODULE, Reason]),
    io:format("StateName: ~p~nStateData: ~p~n",  [StateName, StateData]),
    ok.
