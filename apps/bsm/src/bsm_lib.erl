-module(bsm_lib).

-include_lib("bsm_objects.hrl").

-export([add_entity/1, add_relation/1, rec_event/1]).



%-------------------------------------------------------------------------------
% Working with Local ETS
%-------------------------------------------------------------------------------
add_entity(Entity) ->
	Rec = rec_entity(Entity),
	ets:insert( entid, { Rec#entity.entid, Rec#entity{} } ),
    ets:insert( evtid, { Rec#entity.evtid, Rec#entity{} } ).

add_relation(Relation) ->
	Rec = rec_relation(Relation),
	ets:insert( reldst, { Rec#relation.dstid, Rec#relation.srcid, Rec#relation.attr } ),
    ets:insert( relsrc, { Rec#relation.srcid, Rec#relation.dstid, Rec#relation.attr } ).

%-------------------------------------------------------------------------------
% Record from Clause for Entity
%-------------------------------------------------------------------------------
rec_entity({EntId, Name, Type, Sev, EvtId}) ->
	{ Image, Color, _EventType } = bsm_worker:get_img_color(Type, Sev),
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
             };
rec_entity({Type, EntId, Name, Org}) when Type =:= "organization" ->
	#entity{ entid			= EntId,
             name           = Name,
             type           = Type,
			 organization	= Org
             }.

%-------------------------------------------------------------------------------
% Record from Clause for Relation
%-------------------------------------------------------------------------------
rec_relation({Dst, Src, Attr}) ->
	#relation{ srcid		= Src,
			   dstid		= Dst,
			   attr			= Attr
			   }.

%-------------------------------------------------------------------------------
% Record from Clause for Event
%-------------------------------------------------------------------------------
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