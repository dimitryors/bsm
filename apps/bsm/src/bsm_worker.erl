-module(bsm_worker).

-include_lib("bsm_objects.hrl").
-behaviour(gen_server).

%% Application callbacks
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {name}).

%%====================================================================
%% API
%%====================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

start(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(Name) ->
    %io:format("Worker name: ~n", Name),
    State = #state{ name = Name },
    {ok, State}.

%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
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