-record(entity, {
    entid               :: integer(),
    name            	:: string(),
    type            	:: string(),
    severity        	:: integer(),
    evtid               :: integer(),
    timestamp           :: string(),
    service = []    	:: list(),
    color               :: string(),
    img                 :: string(),
    events = []     	:: list(),
    appgroup = []       :: list(),
    appid               :: integer(),
    organization = []	:: list()
}).

-record(relation, {
    srcid           :: integer(),
    dstid           :: integer(),
    attr = 0        :: integer()
}).

-record(event, {
    evtid           :: integer(),
    alert_key       :: integer(),
    agent           :: string(),
    node            :: string(),
    node_alias      :: string(),
    summary         :: string(),
    severity        :: integer(),
    type            :: string(),
    occurance_ts    :: string(),
    iteration_ts    :: string(),
    color           :: string(),
    srcid = 0       :: integer()
}).
