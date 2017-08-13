-module(bsm_worker).

-include_lib("bsm_objects.hrl").
-behaviour(gen_server).

%% Application callbacks
-export([start_link/1, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_img_color/2, enreach_entity/1]).

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

handle_cast({direction_out, EtsName, Rec}, State) ->
    calc_entity_state({EtsName, Rec}),
    {noreply, State};
handle_cast({direction_in, ObjType}, State) ->
	enreach_entity({ObjType}),
    {noreply, State};
handle_cast({direction_in, ObjType, Event}, State) ->
	enreach_entity({ObjType, Event}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%-------------------------------------------------------------------------------
% Calculate current Entity State and send event with changes next related entity
%-------------------------------------------------------------------------------
calc_entity_state({EtsName, Rec}) ->
		% Search for Entity by EventId
		case ets:lookup(EtsName, Rec#event.evtid) of
			[ {_Id, Entity} | _T ] ->
				% Add Event to Events Array of entity
				{_, Events} = case Rec#event.type of
									"PROBLEM" -> allocate_event({Rec#event{}, Entity#entity.events});
									"OK"	  -> deallocate_event({Rec#event{}, Entity#entity.events})
							  end,
				% Get max severity attr from Events List
				Severity = get_max_severity(Events),
				% Get Image and Color attr 
				{ Image, Color, NewEventType } = get_img_color(Entity#entity.type, Severity),
				% Enreach Entity
				NewEntity = Entity#entity{	severity	= Severity,
											events		= Events,
											img			= Image,
											color		= Color
											},
				% Write NewEntity to ETS
				bsm_lib:add_entity(NewEntity),


				% Make Event
				NewEvent = #event{	node = NewEntity#entity.name,
									summary = "Error reason " ++ NewEntity#entity.type ++": " ++ NewEntity#entity.name ++ " " ++ NewEventType,
									severity = Severity,
									type = NewEventType
									},
				% Get recipient
				Relations = ets:lookup(relsrc, NewEntity#entity.entid),
				% Send messages to Events Load Balancing
				[ bsm_app:event_worker_out({entid,NewEvent#event{ evtid = Dst, srcid = Src }}) 
					|| 
					{Src, Dst, _Attr} <- Relations 
				];
			[] -> {Rec#event.evtid, not_found}
		end.

%-------------------------------------------------------------------------------
% EventList for KE
%-------------------------------------------------------------------------------
allocate_event({Event, EventsList}) ->
	case lists:member(Event#event.srcid, [ Rec#event.srcid || Rec <- EventsList]) of
		true	-> {exists, EventsList};
		false	-> {ok, [Event|EventsList]}
	end.
deallocate_event({Event, EventsList}) ->
	{ok, [ Rec#event{} || Rec <- EventsList, Event#event.srcid =/= Rec#event.srcid ]}.

%-------------------------------------------------------------------------------
% Return Max Severity value for EventList
%-------------------------------------------------------------------------------
get_max_severity([]) 		 -> -1;
get_max_severity(ListEvents) -> lists:max( [ Sev#event.severity || Sev <- ListEvents] ).

%-------------------------------------------------------------------------------
% Generate KE ImageType, Color and EventType  
%-------------------------------------------------------------------------------
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


%-------------------------------------------------------------------------------
% Enreach #entity{ organization } or #entity{ service }
%-------------------------------------------------------------------------------

% -spec enreach_entity(
% 	{ ObjType :: "organization" | "service" } 
% 	|
% 	{ ObjType :: "organization" | "service", Event :: #event{} }
% 	|
% 	{ ObjType :: "organization" | "service", ObjList :: list() }
% ) -> ok.


%-------------------------------------------------------------------------------
% Init all Entities with Type =:= "organization"
%-------------------------------------------------------------------------------
enreach_entity({ObjType}) when ObjType =:= "organization" ->
	% Init all Entities with Type =:= "organization"
	EntityList = ets:match(entid, {'_', #entity{	type			= ObjType,
													entid			= '$0',
													name			= '$1',
													severity		= '_',
													evtid			= '_',
													timestamp		= '_',
													service			= '_',
													color			= '_',
													img				= '_',
													events			= '_',
													appgroup		= '_',
													appid			= '_',
													organization	= '$2'
												}
									}),
	% Process found EntityList with Type =:= "organization"
	enreach_entity({ObjType, EntityList});
%-------------------------------------------------------------------------------
% Init all Entities with Type =:= "service"
%-------------------------------------------------------------------------------
enreach_entity({ObjType}) when ObjType =:= "service"->
	% Init all Entities with Type =:= "service"
	EntityList = ets:match(entid, {'_', #entity{	type			= ObjType,
													entid			= '$0',
													name			= '$1',
													severity		= '_',
													evtid			= '_',
													timestamp		= '_',
													service			= '$2',
													color			= '_',
													img				= '_',
													events			= '_',
													appgroup		= '_',
													appid			= '_',
													organization	= '_'
												}
									}),
	% Process found EntityList with Type =:= "service"
	enreach_entity({ObjType, EntityList});
%-------------------------------------------------------------------------------
% Process EntityList is null, stop loop
%-------------------------------------------------------------------------------
enreach_entity({_ObjType, []}) -> ok;
%-------------------------------------------------------------------------------
% Process EntityList with Type =:= "organization"
%-------------------------------------------------------------------------------
enreach_entity({ObjType, [EntityHead | EntityTail]}) when ObjType =:= "organization" ->
	% Parse Entity
	[EntId, ObjName, ObjList] = EntityHead,
	% Allocate Organization
	ListAllocated = allocate_enttype({ObjName, ObjList}),
	% Find entity and update
	[ {_Id, Rec} ] = ets:lookup(entid, EntId),
	bsm_lib:add_entity(Rec#entity{ organization = ListAllocated }),
	% Create new event
	NewEvent = #event{ node = Rec#entity.name },
	% Find src entity's
	Relations = ets:lookup(reldst, Rec#entity.entid),
	% Snd event to src entity's
	[ bsm_app:event_worker_in({ObjType, NewEvent#event{ evtid = Src, srcid = Dst }}) || {Dst, Src, _Attr} <- Relations ],
	% Loop enreach_organization()			
	enreach_entity({ObjType, EntityTail});
%-------------------------------------------------------------------------------
% Process EntityList with Type =:= "service"
%-------------------------------------------------------------------------------
enreach_entity({ObjType, [EntityHead | EntityTail]}) when ObjType =:= "service" ->
	% Parse Entity
	[EntId, ObjName, ObjList] = EntityHead,
	% Allocate Organization
	ListAllocated = allocate_enttype({ObjName, ObjList}),
	% Find entity and update
	[ {_Id, Rec} ] = ets:lookup(entid, EntId),
	bsm_lib:add_entity(Rec#entity{ service = ListAllocated }),
	% Create new event
	NewEvent = #event{ node = Rec#entity.name },
	% Find src entity's
	Relations = ets:lookup(reldst, Rec#entity.entid),
	% Snd event to src entity's
	[ bsm_app:event_worker_in({ObjType, NewEvent#event{ evtid = Src, srcid = Dst }}) || {Dst, Src, _Attr} <- Relations ],
	% Loop enreach_organization()			
	enreach_entity({ObjType, EntityTail});
%-------------------------------------------------------------------------------
% Process Enreach Entity from Event with Type =:= "organization"
%-------------------------------------------------------------------------------
enreach_entity({ObjType, Event}) when ObjType =:= "organization", is_record(Event, event) =:= true ->
	% Find Entity to change
	[ {_Id, Entity} | _T ] = ets:lookup(entid, Event#event.evtid),
	% Allocate Organization
	OrgAllocated = allocate_enttype({Event#event.node, Entity#entity.organization}),
	% Update Entity
	bsm_lib:add_entity( Entity#entity{ organization = OrgAllocated }),
	% Create new event
	NewEvent = #event{ node = Event#event.node },
	% Find src entity's
	Relations = ets:lookup(reldst, Entity#entity.entid),
	% Snd event to src entity's
	[ bsm_app:event_worker_in({ObjType, NewEvent#event{ evtid = Src, srcid = Dst }}) || {Dst, Src, _Attr} <- Relations ],
	ok;
%-------------------------------------------------------------------------------
% Process Enreach Entity from Event with Type =:= "service"
%-------------------------------------------------------------------------------
enreach_entity({ObjType, Event}) when ObjType =:= "service", is_record(Event, event) =:= true ->
	% Find Entity to change
	[ {_Id, Entity} | _T ] = ets:lookup(entid, Event#event.evtid),
	% Allocate Organization
	SvcAllocated = allocate_enttype({Event#event.node, Entity#entity.service}),
	% Update Entity
	bsm_lib:add_entity( Entity#entity{ service = SvcAllocated }),
	% Create new event
	NewEvent = #event{ node = Event#event.node },
	% Find src entity's
	Relations = ets:lookup(reldst, Entity#entity.entid),
	% Snd event to src entity's
	[ bsm_app:event_worker_in({ObjType, NewEvent#event{ evtid = Src, srcid = Dst }}) || {Dst, Src, _Attr} <- Relations ],
	ok;
%-------------------------------------------------------------------------------
% Catch Error
%-------------------------------------------------------------------------------
enreach_entity(ObjType) -> io:format("Bad ObjType: ~n", ObjType).


allocate_enttype({ObjName, ObjList}) ->
	case lists:member(ObjName, ObjList) of
		true	-> ObjList;
		false	-> [ObjName|ObjList]
	end.