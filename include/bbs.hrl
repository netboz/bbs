-define(ONTO_STORE,onto_store).

-record(agent,{
  name :: binary(),
  aid_entries,
  startup_ontologies = [] :: list()}).
