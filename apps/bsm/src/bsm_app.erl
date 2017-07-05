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
-export([entity/1, relation/1, event/1, fill_ets/0, snd_event/0, rec_entity/1, rec_event/1, allocate_event/1, deallocate_event/1, dev/0]).

-define(SERVER, ?MODULE).
-record(state, {}).


%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(_StartType, _StartArgs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    bsm_sup:start_link().

init([]) ->
    entid  = ets:new(entid,  [set, named_table]),
    evtid  = ets:new(evtid,  [set, named_table]),
    reldst = ets:new(reldst, [bag, named_table]),
    relsrc = ets:new(relsrc, [bag, named_table]),
    {ok, #state{}}.

dev() ->
   start_link(),
   fill_ets(),
   sys:trace(bsm_app, true),
   snd_event().

%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

entity(Entity) 	  -> gen_server:cast(bsm_app, {add_ent, Entity}).
relation(Relation) -> gen_server:cast(bsm_app, {add_rel, Relation}).

-spec event({Direction :: direction_out | direction_in,
	     EtsName   :: evtid | entid,
	     Event     :: { EvtId :: integer(),
			    AlertKey :: integer(),
			    Agent ::  string(),
			    Node :: string(),
			    NodeAlias :: string(),
			    Summary :: string(),
			    Sev :: integer(),
			    Type :: string(),
			    OccuranceTs :: string(),
			    IterationTs :: integer()
			  }
	    }) -> ok.
event({Direction, EtsName, Event}) -> gen_server:cast(bsm_app, {Direction, EtsName, rec_event(Event)}).


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({add_ent, Entity}, State) ->
	add_entity(Entity),
    {noreply, State};
handle_cast({add_rel, Relation}, State) ->
	add_relation(Relation),
    {noreply, State};
handle_cast({direction_out, EtsName, Rec}, State) when Rec#event.type =:= "PROBLEM" ->
		% Search for Entity by EventId
		[ {_Id, Entity} | _T ] = ets:lookup(EtsName, Rec#event.evtid),
		% Add Event to Events Array of entity
		{_, Events} = allocate_event({Rec#event{}, Entity#entity.events}),
		% Get max severity attr from Events List
		Severity = get_max_severity(Events),
		% Get Image and Color attr 
		{ Image, Color, NewEventType } = get_img_color(Entity#entity.type, Severity),
		% Enreach Entity
		NewEntity = Entity#entity{ severity = Severity,
					   events = Events,
					   img = Image,
					   color = Color
					 },
		% Write NewEntity to ETS
		add_entity(NewEntity),
		% Make Event
		NewEvent = #event{ node = NewEntity#entity.name,
				   summary = "Error reason " ++ NewEntity#entity.type ++": " ++ NewEntity#entity.name ++ " " ++ NewEventType,
				   severity = Severity,
				   type = NewEventType
				},
		Relations = ets:lookup(relsrc, NewEntity#entity.entid),
		[ event({direction_out, entid,NewEvent#event{ evtid = Dst, srcid = Src }}) || {Src, Dst, _Attr} <- Relations ],
    {noreply, State};
handle_cast({direction_out, EtsName, Rec}, State) when Rec#event.type =:= "OK" ->
		% Search for Entity by EventId
		[ {_Id, Entity} | _T ] = ets:lookup(EtsName, Rec#event.evtid),
		% Remove Event from Events Array of entity
		{_, Events} = deallocate_event({Rec#event{}, Entity#entity.events}),
		% Get max severity attr from Events List
		Severity = get_max_severity(Events),
		% Get Image and Color attr 
		{ Image, Color, NewEventType } = get_img_color(Entity#entity.type, Severity),
		% Enreach Entity
		NewEntity = Entity#entity{ severity = Severity,
					   events = Events,
					   img = Image,
					   color = Color
					 },
		% Write NewEntity to ETS
		add_entity(NewEntity),
		% Make Event
		NewEvent = #event{ node = NewEntity#entity.name,
				   summary = "Error reason " ++ NewEntity#entity.type ++ " " ++ NewEventType,
				   severity = Severity,
			           type = NewEventType
				 },
		Relations = ets:lookup(relsrc, NewEntity#entity.entid),
		[ event({direction_out, entid, NewEvent#event{ evtid = Dst, srcid = Src }}) || {Src, Dst, _Attr} <- Relations ],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_entity(Entity) ->
	Rec = rec_entity(Entity),
	ets:insert( entid, { Rec#entity.entid, Rec#entity{} } ),
        ets:insert( evtid, { Rec#entity.evtid, Rec#entity{} } ).

add_relation(Relation) ->
	Rec = rec_relation(Relation),
	ets:insert( reldst, { Rec#relation.dstid, Rec#relation.srcid, Rec#relation.attr } ),
        ets:insert( relsrc, { Rec#relation.srcid, Rec#relation.dstid, Rec#relation.attr } ).

allocate_event({Event, EventsList}) ->
	case lists:member(Event#event.srcid, [ Rec#event.srcid || Rec <- EventsList]) of
		true	-> {exists, EventsList};
		false	-> {ok, [Event|EventsList]}
	end.
deallocate_event({Event, EventsList}) ->
	{ok, [ Rec#event{} || Rec <- EventsList, Event#event.srcid =/= Rec#event.srcid ]}.

get_max_severity([]) 		 -> -1;
get_max_severity(ListEvents) -> lists:max( [ Sev#event.severity || Sev <- ListEvents] ).

get_img_color(EntityType, EntitySeverity) ->
	case EntityType of
		"service" 		-> ImageType = "BusinessService_";
		"application"	-> ImageType = "ApplicationServer_";
		"node" 			-> ImageType = "DataBaseServer_";
		"metric"		-> ImageType = "Router_";
		_ 				-> ImageType = ""
	end,
	case EntitySeverity of
		5 -> Image = ImageType ++ "Critical.png", Color = "#F44336", _SevChar = "CRITICAL", EventType = "PROBLEM";
		4 -> Image = ImageType ++ "Major.png", Color = "#FF9800", _SevChar = "MAJOR", EventType = "PROBLEM";
		3 -> Image = ImageType ++ "Warning.png", Color = "#FFEB3B", _SevChar = "MINOR", EventType = "PROBLEM";
		2 -> Image = ImageType ++ "Normal.png", Color = "#4CAF50", _SevChar = "WARNING", EventType = "OK";
		1 -> Image = ImageType ++ "Normal.png", Color = "#4CAF50", _SevChar = "INFO", EventType = "OK";
		_ -> Image = ImageType ++ "Normal.png", Color = "#4CAF50", _SevChar = "NORMAL", EventType = "OK"
	end,
	{ Image, Color, EventType }.

rec_entity({EntId, Name, Type, Sev, EvtId}) ->
	{ Image, Color, _EventType } = get_img_color(Type, Sev),
	#entity{ entid			= EntId,
             name           = Name,
             type           = Type,
             severity       = Sev,
             evtid			= EvtId,
			 img			= Image,
			 color			= Color
             };
rec_entity({entity, EntId, Name, Type, Sev, EvtId, Ts, Svc, Color, Img, Events, AppGroup, AppId, Org }) ->
	#entity{ entid			= EntId,
             name           = Name,
             type           = Type,
             severity       = Sev,
             evtid			= EvtId,
			 timestamp	    = Ts,
			 service        = Svc,
			 color			= Color,
			 img			= Img,
			 events			= Events,
			 appgroup		= AppGroup,
			 appid			= AppId,
			 organization	= Org
             }.

rec_relation({Dst, Src, Attr}) ->
	#relation{ srcid		= Src,
			   dstid		= Dst,
			   attr			= Attr
			   }.

rec_event({EvtId, AlertKey, Agent, Node, NodeAlias, Summary, Sev, Type, OccuranceTs, IterationTs}) ->
    #event{ evtid			= EvtId,
			alert_key		= AlertKey,
			agent			= Agent,
			node			= Node,
			node_alias		= NodeAlias,
			summary			= Summary,
			severity		= Sev,
			type			= Type,
			occurance_ts	= OccuranceTs,
			iteration_ts	= IterationTs
			};
rec_event({event, EvtId, AlertKey, Agent, Node, NodeAlias, Summary, Sev, Type, OccuranceTs, IterationTs, Color, SrcId}) ->
    #event{ evtid			= EvtId,
			alert_key		= AlertKey,
			agent			= Agent,
			node			= Node,
			node_alias		= NodeAlias,
			summary			= Summary,
			severity		= Sev,
			type			= Type,
			occurance_ts	= OccuranceTs,
			iteration_ts	= IterationTs,
			color			= Color,
			srcid			= SrcId
			}.

%%====================================================================
%% To be moved external functions
%%====================================================================

fill_ets() ->
    [ entity(Entity)     || Entity   <- get_entity()],
    [ relation(Relation) || Relation <- get_relation()].

snd_event() ->
    [ event({direction_out, evtid, Event}) || Event <- get_event()].

get_entity() ->
	% {entity_id, name, type, severity, service, event_id, events, app_group, organization}
	% {entity_id, name, type, severity, event_id}
	[
		{100000, "CROC", "organization", -1, 0},
		{104408, "Накопление сообщений в очереди SYSTEM.BROKER.AGGR.TIMEOUT", "metric",	-1, 1000366},
		{103915, "s-kssh MQ", "application", -1, 100055},
		{104383, "Накопление сообщений в очереди EVENT.LOGGING.REPEAT",	"metric", -1, 1000363},
		{104583, "КСШ", "service", -1, 0},
		{104385, "Накопление сообщений в очереди EVENT.LOGGING", "metric", -1, 1000362},
		{104188, "Накопление сообщений в очереди Adapter.KIAP.DBWrite.TECH.ERROR", "metric", -1, 1000352},
		{104059, "Директория /mnt/expressdata не смонтирована на {HOST.NAME}", "metric", -1, 1000345},
		{104411, "Накопление сообщений в очереди SYSTEM.DEAD.LETTER.QUEUE", "metric", -1, 1000367},
		{104387, "Накопление сообщений в очереди EVENT.LOGGING.REPEAT", "metric", -1, 1000364},
		{104113, "Накопление сообщений в очереди Adapter.IAO.DBWrite.DATA.ERROR", "metric", -1, 1000347},
		{104104, "Накопление сообщений в очереди Adapter.IAO.DBWrite.REPEAT", "metric",	-1,	1000348},
		{104417, "Новая запись в /var/log/user.log {ITEM1.LASTVALUE}", "metric", -1, 1000369},
		{104450, "Проблема с Srv.RezDataLoading.ERROR", "metric", -1, 1000375},
		{104200, "Накопление сообщений в очереди Adapter.LNSI.GetData.ERROR", "metric",	-1, 1000353},
		{104157, "Накопление сообщений в очереди Adapter.IAO.DBWrite.TECH.ERROR", "metric", -1, 1000349},
		{104419, "Накопление сообщений в очереди RECODE.ERROR", "metric", -1, 1000365},
		{104095, "Накопление сообщений в очереди Adapter.EP.ERROR", "metric", -1, 1000346},
		{103925, "s-ksshdb MS SQL", "application", -1, 100061},
		{104336, "Накопление сообщений в очереди ALARM.TSOPPD",	"metric", -1, 1000357},
		{101222, "s-ksshdb", "node", -1, 100022},
		{104155, "Накопление сообщений в очереди Adapter.KIAP.DBWrite.DATA.ERROR", "metric", -1, 1000350},
		{104317, "Накопление сообщений в очереди Adapter.Notifications.SMSGate.ERROR", "metric", -1, 1000355},
		{104442, "Новая запись в AMQERR01.LOG {ITEM1.LASTVALUE}", "metric", -1, 1000370},
		{104426, "Отсутствуют процессы MQ на {HOST.NAME} более 5 минут", "metric", -1, 1000374},
		{103937, "s-kssh SERVICE", "application", -1, 100057},
		{104162, "Накопление сообщений в очереди Adapter.KIAP.DBWrite.REPEAT", "metric", -1, 1000351},
		{104004, "s-kssh TSOPPD", "application", -1, 100058},
		{104372, "Накопление сообщений в очереди App.LNSI.putUpdateTable.REQUEST (10min)", "metric", -1, 1000360},
		{103941, "s-kssh NSI", "application", -1, 100056},
		{104375, "Накопление сообщений в очереди App.LNSI.putUpdateTable.REQUEST.DLQ", "metric", -1, 1000361},
		{104302, "Накопление сообщений в очереди ALARM.LNSI", "metric", -1,	1000356},
		{104424, "Обнаружены старые сообщения в очереди App.LNSI.putUpdateTable.REQUEST", "metric", -1, 1000373},
		{104415, "Обнаружены старые сообщения в очереди App.LNSI.PutData.REQUEST", "metric", -1, 1000372},
		{104364, "Накопление сообщений в очереди App.LNSI.PutData.REQUEST (10min)", "metric", -1, 1000359},
		{104400, "Обнаружены старые сообщения в очереди App.LNSI.PutBigData.REQUEST", "metric",	-1, 1000371},
		{104396, "Нет данных мониторинга очередей в течение 10 минут на {HOST.NAME}", "metric",	-1, 1000368},
		{101224, "s-kssh", "node", -1, 100021},
		{104334, "Накопление сообщений в очереди App.LNSI.PutBigData.REQUEST", metric, -1, 1000358},
		{104274, "Накопление сообщений в очереди Adapter.LNSI.PutData.IN", "metric", -1, 1000354}
	].

get_relation() ->
	% {_source.dstId, _source.srcId, attr }
	[
		{103937, 104426, 0},
		{104004, 101224, 0},
		{103937, 101224, 0},
		{103925, 101222, 0},
		{103941, 104302, 0},
		{103941, 104334, 0},
		{103941, 104364, 0},
		{103941, 104375, 0},
		{103941, 104415, 0},
		{103941, 104274, 0},
		{103941, 104372, 0},
		{103941, 104396, 0},
		{103941, 104400, 0},
		{103915, 104157, 0},
		{103915, 104200, 0},
		{103915, 104317, 0},
		{103915, 104387, 0},
		{103915, 104417, 0},
		{103915, 104450, 0},
		{103915, 104095, 0},
		{103915, 104113, 0},
		{103915, 104155, 0},
		{103915, 104162, 0},
		{103915, 104188, 0},
		{103915, 104364, 0},
		{103915, 104383, 0},
		{103915, 104385, 0},
		{103915, 104408, 0},
		{103915, 104419, 0},
		{103915, 104442, 0},
		{103941, 104424, 0},
		{103915, 104059, 0},
		{103915, 104104, 0},
		{103915, 104302, 0},
		{103915, 104334, 0},
		{103915, 104336, 0},
		{103915, 104411, 0},
		{103941, 101224, 0},
		{103915, 104424, 0},
		{103915, 104274, 0},
		{103915, 104372, 0},
		{103915, 104396, 0},
		{103915, 104400, 0},
		{103915, 104426, 0},
		{103915, 101224, 0},
		{103915, 104375, 0},
		{103915, 104415, 0},
		{101236, 101222, 0},
		{104583, 103925, 0},
		{104583, 104004, 0},
		{104583, 103915, 0},
		{104583, 103937, 0},
		{104583, 103941, 0},
		{104585, 103941, 0},
		{185773, 101224, 0},
		{101230, 101222, 0},
		{185791, 101224, 0},
		{185781, 101224, 0},
		{100000, 104583, 0}
	].

get_event() ->
	% {EvtId, AlertKey, Agent, Node, NodeAlias, Summary, Sev, Type, OccuranceTs, IterationTs}
    [
        {1000375, 375, "zabbix", "s-kssh", "s-kssh", "Проблема с Srv.RezDataLoading.ERROR", 5, "PROBLEM", "2017-06-26T07:09:42", "2017-06-26T07:09:42"},
	{1000349, 349, "zabbix", "s-kssh", "s-kssh", "Накопление сообщений в очереди Adapter.IAO.DBWrite.TECH.ERROR", 5, "PROBLEM", "2017-06-26T07:09:42", "2017-06-26T07:09:42"},
	{1000353, 375, "zabbix", "s-kssh", "s-kssh", "Накопление сообщений в очереди Adapter.LNSI.GetData.ERROR", 5, "PROBLEM", "2017-06-26T07:09:42", "2017-06-26T07:09:42"}
    ].
