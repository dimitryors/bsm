%%%-------------------------------------------------------------------
%% @doc bsm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, attach_worker/1, detach_worker/1]).

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
    ChildSpecList = [],
    SupFlags = #{   strategy    => one_for_one,
                    intensity   => 10,
                    period      => 3600
                },
    {ok, { SupFlags, ChildSpecList} }.

%%====================================================================
%% Internal functions
%%====================================================================

attach_worker(Name) ->
    ChildSpec = child(bsm_worker, Name),
    supervisor:start_child(?MODULE, ChildSpec).

detach_worker(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).


child(Module, Name) ->
    #{  id          => Name,
        start       => {Module, start_link, [Name]},
        restart     => transient,
        shutdown    => 2000,
        type        => worker,
        modules     => [Module]
    }.