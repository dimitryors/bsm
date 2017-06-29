-record(entity, {
    entity_id       	:: integer(),
    name            	:: string(),
    type            	:: string(),
    severity        	:: integer(),
    service = []    	:: list(),
    event_id        	:: integer(),
    events = []     	:: list(),
    app_group = []  	:: list(),
    organization = []	:: list()
}).

-record(relation, {
    src_id          :: integer(),
    dst_id          :: integer(),
    attr = 0        :: integer()
}).

-record(event, {
    event_id        :: integer(),
    alert_key       :: integer(),
    agent           :: string(),
    node            :: string(),
    node_alias      :: string(),
    summary         :: string(),
    severity        :: integer(),
    type            :: string(),
    color           :: string(),
    occurance_ts    :: string(),
    iteration_ts    :: string()
}).
