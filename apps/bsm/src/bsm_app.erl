%%%-------------------------------------------------------------------
%% @doc bsm public API
%% @end
%%%-------------------------------------------------------------------

-module(bsm_app).

-behaviour(application).

-include_lib("bsm_objects.hrl").
-behaviour(gen_server).

%% Application callbacks
-export([start_link/0, start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    Entity = #entity{},
    Relation = #relation{},
    Event = #event{},
    Ent1 = {104583,
            Entity#entity{  
                    entity_id = 104583,
                    name = "KSH",
                    type = "service",
                    severity = -1,
                    service = [],
                    event_id = 0,
                    events = [],
                    app_group = []
                }
            },
    TabId = ets:new(entity, [set, named_table]),
    ets:insert(entity, Ent1),
    io:format("~n", ets:lookup(entity, 104583)),
    {ok, #state{}}.

start(_StartType, _StartArgs) ->
    bsm_sup:start_link().

%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.