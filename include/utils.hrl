%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Lager

-define(DEBUG(Template, Args),
  lager:debug(Template, Args)).

-define(INFO_MSG(Template, Args),
  lager:info(Template, Args)).

-define(WARNING_MSG(Template, Args),
  lager:warning(Template, Args)).

-define(ERROR_MSG(Template, Args),
  lager:error(Template, Args)).

