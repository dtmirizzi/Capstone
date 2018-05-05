-module(super).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init(_Args) ->
    init(_Arguments) ->
    DbServerOtp = {dtdb,                %% Id
                   {dtdb, start, []},   %% child process
                   permanent,           %% restart
                   30000,               %% shutdown (ms)
                   worker,              %% type
                   [dtdb]},             %% required modules
    {ok,
     {{one_for_one,           %% terminate all children and restart
       5,                     %% max of n restarts in MaxSeconds
       3600},                 %% MaxSeconds (s)
      [DbServerOtp]}}.
