-define(ONTO_STORE,onto_store).

-record(agent,{name, aid_entries, boot, startup_onts = []}).
