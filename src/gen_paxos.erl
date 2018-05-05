%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen Paxos Alg
%% By DT Mirizzi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(gen_paxos).

-export([ask/1, ask/2, start_link/2,
         stop/0, clear/0]).

-define( DEFAULT_COORDINATOR_NUM, 3 ).

start_link( InitN, Others )->
    io:format( "starting ~p agent...", [?MODULE] ),
    Pongs=lists:filter(fun(X)->
                               case X of pong -> true; _-> false end
                       end ,
                       lists:map(
                         fun(Other)-> net_adm:ping(Other) end,
                         Others )),
    io:format( "~p nodes ponged.~n", [length( Pongs )] ),
    start_link( InitN, Others, ?DEFAULT_COORDINATOR_NUM ).

start_link( _InitN, _Others, 0 )->     [];
start_link( InitN, Others, NumCoordinators )->
    Pid = spawn_link( fun()-> coordinator(InitN, Others) end ),
    ok=io:format( "starting coordinator: ~p (~p).~n", [get_process_name_from_int(NumCoordinators), Pid] ),
    true=register( get_process_name_from_int(NumCoordinators), Pid ),
    [Pid|start_link( InitN, Others, NumCoordinators-1)].

get_process_name_from_int( N )->
    list_to_atom( "coordinator" ++ integer_to_list(N) ).

get_process_name_from_key( Key )->
    get_process_name_from_int( erlang:phash( Key, ?DEFAULT_COORDINATOR_NUM ) ). % 1...?DEFAULT_COORDINATOR_NUM

stop()->
    stop( ?DEFAULT_COORDINATOR_NUM ).

stop(0)-> ok;
stop(N)->
    Coordinator = get_process_name_from_int( N ),
    Coordinator ! {self(), stop, normal},
    stop(N-1).


ask(Key)->    ask(Key,void).

ask(Key, Value)->
    Coordinator = get_process_name_from_key( Key ),
    Coordinator ! {self(), ask, { Key, Value }},
    receive
        {_From, result, {Key, Value} }->
            Value;
        {_From, result, {Key, Other} }->
            Other
    end.

clear()->    clear( ?DEFAULT_COORDINATOR_NUM ).

clear(0)-> ok;
clear(N)->
    Coordinator = get_process_name_from_int( N ),
    Coordinator ! {self(), clear, normal},
    clear(N-1).

coordinator( InitN, Others )->
    receive
        {From, ask, {Key, void}}->
            From ! {self(), result, {Key, get( Key )}};
        {From, ask, {Key, Value}}->
            case get( Key ) of
                undefined->
                    io:format("starting active paxos: ~p~n", [{From, ask, {Key,Value}}]),
                    lists:map( fun(Node)->
                                       io:format("starting message to: ~p ! ~p~n",
                                                 [{get_process_name_from_key(Key), Node},
                                                  {self(), suggest, {Key, Value}}]),
                                       {get_process_name_from_key(Key), Node} ! {self(), suggest, {Key,Value} }
                               end,
                               Others),
                    paxos_fsm:start( Key, InitN, Value, Others, [self(), From]);
                ResultValue->
                    From ! {self(), result, {Key, ResultValue} }
            end;
        {_From, suggest, {Key, Value}}->
            io:format("starting passive paxos: ~p~n", [{_From, subject, {Key,Value}}]),
            paxos_fsm:start( Key, InitN, Value, Others, [self()] );
        {_From, result, {Key, Value}}->
            put( Key, Value );
        {_From, stop, normal}->
            exit( stop );
        Other ->
            io:format("~p~n", [{error, {unknown_massage, Other}}])
    end,
    coordinator( InitN, Others ).
