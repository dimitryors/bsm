%%%-------------------------------------------------------------------
%% @doc bsm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecList = [   child(worker_sup, supervisor),
                        child(bsm_app, worker)
                    ],
    SupFlags = #{   strategy    => rest_for_one,
                    intensity   => 2,
                    period      => 3600
                },
    {ok, { SupFlags, ChildSpecList} }.

%%====================================================================
%% Internal functions
%%====================================================================

child(Module, Type) ->
    #{  id          => Module,
        start       => {Module, start_link, []},
        restart     => permanent,
        shutdown    => 2000,
        type        => Type,
        modules     => [Module]
    }.