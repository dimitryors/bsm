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


get_entity() ->
	% {entity_id, name, type, severity, service, event_id, events, app_group, organization}
	[
		{104408, "Накопление сообщений в очереди SYSTEM.BROKER.AGGR.TIMEOUT", "metric",	-1, [], 1000366, [], [], []},
		{103915, "s-kssh MQ", "application", -1, [], 100055, [], [], []},
		{104383, "Накопление сообщений в очереди EVENT.LOGGING.REPEAT",	"metric", -1, [], 1000363, [], [], []},
		{104583, "КСШ", "service", -1, [], 0, [], [], []},
		{104385, "Накопление сообщений в очереди EVENT.LOGGING", "metric", -1, [], 1000362, [], [], []},
		{104188, "Накопление сообщений в очереди Adapter.KIAP.DBWrite.TECH.ERROR", "metric", -1, [], 1000352, [], [], []},
		{104059, "Директория /mnt/expressdata не смонтирована на {HOST.NAME}", "metric", -1, [], 1000345, [], [], []},
		{104411, "Накопление сообщений в очереди SYSTEM.DEAD.LETTER.QUEUE", "metric", -1, [], 1000367, [], [], []},
		{104387, "Накопление сообщений в очереди EVENT.LOGGING.REPEAT", "metric", -1, [], 1000364, [], [], []},
		{104113, "Накопление сообщений в очереди Adapter.IAO.DBWrite.DATA.ERROR", "metric", -1, [], 1000347, [], [], []},
		{104104, "Накопление сообщений в очереди Adapter.IAO.DBWrite.REPEAT", "metric",	-1, [],	1000348, [], [], []},
		{104417, "Новая запись в /var/log/user.log {ITEM1.LASTVALUE}", "metric", -1, [], 1000369, [], [], []},
		{104450, "Проблема с Srv.RezDataLoading.ERROR", "metric", -1, [], 1000375, [], [], []},
		{104200, "Накопление сообщений в очереди Adapter.LNSI.GetData.ERROR", "metric",	-1, [], 1000353, [], [], []},
		{104157, "Накопление сообщений в очереди Adapter.IAO.DBWrite.TECH.ERROR", "metric", -1, [], 1000349, [], [], []},
		{104419, "Накопление сообщений в очереди RECODE.ERROR", "metric", -1, [], 1000365, [], [], []},
		{104095, "Накопление сообщений в очереди Adapter.EP.ERROR", "metric", -1, [], 1000346, [], [], []},
		{103925, "s-ksshdb MS SQL", "application", -1, [], 100061, [], [], []},
		{104336, "Накопление сообщений в очереди ALARM.TSOPPD",	"metric", -1, [], 1000357, [], [], []},
		{101222, "s-ksshdb", "node", -1, [], 100022, [], [], []},
		{104155, "Накопление сообщений в очереди Adapter.KIAP.DBWrite.DATA.ERROR", "metric", -1, [], 1000350, [], [], []},
		{104317, "Накопление сообщений в очереди Adapter.Notifications.SMSGate.ERROR", "metric", -1, [], 1000355, [], [], []},
		{104442, "Новая запись в AMQERR01.LOG {ITEM1.LASTVALUE}", "metric", -1, [], 1000370, [], [], []},
		{104426, "Отсутствуют процессы MQ на {HOST.NAME} более 5 минут", "metric", -1, [], 1000374, [], [], []},
		{103937, "s-kssh SERVICE", "application", -1, [], 100057, [], [], []},
		{104162, "Накопление сообщений в очереди Adapter.KIAP.DBWrite.REPEAT", "metric", -1, [], 1000351, [], [], []},
		{104004, "s-kssh TSOPPD", "application", -1, [], 100058, [], [], []},
		{104372, "Накопление сообщений в очереди App.LNSI.putUpdateTable.REQUEST (10min)", "metric", -1, [], 1000360, [], [], []},
		{103941, "s-kssh NSI", "application", -1, [], 100056, [], [], []},
		{104375, "Накопление сообщений в очереди App.LNSI.putUpdateTable.REQUEST.DLQ", "metric", -1, [], 1000361, [], [], []},
		{104302, "Накопление сообщений в очереди ALARM.LNSI", "metric", -1, [],	1000356, [], [], []},
		{104424, "Обнаружены старые сообщения в очереди App.LNSI.putUpdateTable.REQUEST", "metric", -1, [], 1000373, [], [], []},
		{104415, "Обнаружены старые сообщения в очереди App.LNSI.PutData.REQUEST", "metric", -1, [], 1000372, [], [], []},
		{104364, "Накопление сообщений в очереди App.LNSI.PutData.REQUEST (10min)", "metric", -1, [], 1000359, [], [], []},
		{104400, "Обнаружены старые сообщения в очереди App.LNSI.PutBigData.REQUEST", "metric",	-1, [], 1000371, [], [], []},
		{104396, "Нет данных мониторинга очередей в течение 10 минут на {HOST.NAME}", "metric",	-1, [], 1000368, [], [], []},
		{101224, "s-kssh", "node", -1, [], 100021, [], [], []},
		{104334, "Накопление сообщений в очереди App.LNSI.PutBigData.REQUEST", metric, -1, [], 1000358, [], [], []},
		{104274, "Накопление сообщений в очереди Adapter.LNSI.PutData.IN", "metric", -1, [], 1000354, [], [], []},
	].
