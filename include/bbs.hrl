-define(HORDESUP, 'Elixir.Horde.DynamicSupervisor').
-define(HORDEREG, 'Elixir.Horde.Registry').
-define(HORDISTRIB, 'Elixir.Horde.UniformQuorumDistribution').
-define(BBS_BUBLES_SUP, bbs_bubles_sup).
-define(BBS_BUBBLES_REG, bbs_bubbles_registry).
-define(ONTO_STORE, onto_store).

-record(agent, {
	name :: binary(), 
	startup_ontologies = [] :: list(), 
	tree_node :: term(), 
	parent :: binary()}).
