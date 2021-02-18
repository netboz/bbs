-define(ONTO_STORE,onto_store).

-record(agent,{
  name :: binary(),
  startup_ontologies = [] :: list()}).
