%%%-------------------------------------------------------------------
%%% @author netboz
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. juil. 2015 23:23
%%%-------------------------------------------------------------------
-module(ont_bbs_iiotp).
-author("netboz").

-include("bbs.hrl").
-include("utils.hrl").
-include_lib("erlog/include/erlog_int.hrl").

%% Prolog API

-define(ERLANG_PREDS,
  [
    {{new_acc, 1}, ?MODULE, new_acc},
    {{new_acc, 3}, ?MODULE, new_acc},
    {{new_public_acc, 1}, ?MODULE, new_public_acc},
    {{send_msg_iiotp, 5}, ?MODULE, send_msg_iiotp},
    {{public_acc, 2}, ?MODULE, public_acc},
    {{printable_all_acc, 1}, ?MODULE, printable_all_acc}
  ]).

-define(PROLOG_PREDS, [
  {file, "./ont_bbs_iiotp.pl"}
]).

%% API
-export([external_predicates/0, prolog_predicates/0, namespace/0]).
-export([new_acc/3, new_public_acc/3, send_msg_iiotp/3, public_acc/3, printable_all_acc/3]).

namespace() -> <<"bbs:iiotp">>.

external_predicates() ->
  ?ERLANG_PREDS.


prolog_predicates() ->
  ?PROLOG_PREDS.


%% Those next two are for message reception only

new_acc({_Atom, Acc},Next0, #est{} = St) ->
  AccKey = bbs_utils:generate_random_string(8),
  AgKey = gproc:get_value({p, l, {aid_entries, name}}),
  gproc:reg({p, l, {iiotp, AccKey, AgKey}}),
  erlog_int:unify_prove_body(Acc, {iiotp, AccKey, AgKey},Next0, St);


%% Create a new ACC, and set a hook at message reception
new_acc({_Atom, Acc, Ont, Pred}, Next0, #est{} = St) ->
  ?INFO_MSG("Creating new acc :~p",[{Ont,Pred}]),
  AccKey = bbs_utils:generate_random_string(8),
  AgKey = gproc:get_value({p, l, {aid_entries, name}}),
  gproc:reg({p, l, {iiotp, AccKey, AgKey}}, {Ont, Pred}),
  erlog_int:unify_prove_body(Acc, {iiotp, AccKey, AgKey},Next0, St).

%% react_on(NameSpaceIn, PredPat, NameSpaceOut, PredReact) ->
%% react_on("bbs:iiotp",iiotp_message(Origin, Destination, Message), react_console_message(Origin, Destination, Message)),


new_public_acc({_Atom, Acc}, Next0, #est{} = St) ->
  AgKey = gproc:get_value({p, l, {aid_entries, name}}),
  %% Set stim
  %%ontology:react_on(namespace(), {iiotp_message, public, AgKey, in}, Ont, Pred),

  gproc:reg({p, l, {iiotp, public, AgKey}}),
  erlog_int:unify_prove_body(Acc,{iiotp, public, AgKey}, Next0, St).

%%That's bad
public_acc({_Atom, Agent, Acc}, Next0, #est{} = St) ->
  %%?INFO_MSG("Getting public ACC for :~p", [Agent]),
  erlog_int:unify_prove_body({iiotp, public, Agent}, Acc, Next0, St).

printable_all_acc({_Atom, Accs}, Next0, #est{} = St) ->
  AgKey = gproc:get_value({p, l, {aid_entries, name}}),
  ?INFO_MSG("printable_all_acc ~p",[AgKey]),

  Key = {iiotp, '_', AgKey},
  GProcKey = {'_', '_', Key},
  MatchHead = {GProcKey, '_', '_'},
  Guard = [],
  Result = ['$$'],
  ?INFO_MSG("Before gproc",[]),
  Res = gproc:select([{MatchHead, Guard, Result}]),
  ?INFO_MSG("Res ~p",[[Iiotp || [{_, _, Iiotp}, _, _] <- Res]]),

  ConvRes = lists:map(
    fun({iiotp, Key, Ag}) when is_list(Key) ->
      {iiotp, Key, Ag};
      ({iiotp, Key, Ag}) when is_atom(Key) ->
        {iiotp, atom_to_binary(Key, utf8), Ag} end, [Iiotp || [{_, _, Iiotp}, _, _] <- Res]
  ),
  erlog_int:unify_prove_body(Accs, ConvRes, Next0, St).





send_msg_iiotp({_Atom, SenderAid, DestAid, Acc, Ontology, Msg}, Next0, St) ->
  ?INFO_MSG("Sending message :~p",[{Acc, Ontology, Msg}]),
  ConvPred = variabilise(Msg, []), %St#est.varnames),
  gproc:send({p, l, Acc}, {<<"bbs:iiotp">>, {iiotp_message, Acc, SenderAid, DestAid, Ontology, ConvPred}}),
  R= io_lib:format("~p",[ConvPred]),
  F = lists:flatten(R),
  lager:info("Convpred :~p",[ConvPred]),

  lager:info("Logging :~p",[F]),
  plantuml_logger:log_plant_message(maps:get(name,SenderAid), maps:get(name,DestAid), "ont_bbs_iiotp", ConvPred),
  erlog_int:prove_body(Next0, St).

%%%% TODO: Get rid of this variabilise
%%
variabilise(El, _VarDict) when is_atom(El) orelse is_number(El) orelse is_map(El) ->
  El;
variabilise(El, _VarDict) when is_binary(El) orelse is_pid(El) ->
  El;
variabilise({Key}, VarDict) ->
  VarName = dict:fetch(Key, VarDict),
  {VarName};

variabilise([A | _B] = String, _VarDict) when is_integer(A) ->
  String;

variabilise(El, VarDict) when is_tuple(El) ->
  list_to_tuple(variabilise(tuple_to_list(El), VarDict));

variabilise([A | B], VarDict) ->
  variabilise_list([A | B], VarDict, []);


variabilise([], _VarDict) ->
  [].


variabilise_list([A | B], VarDict, Acc) ->
  Term = variabilise(A, VarDict),
  variabilise_list(B, VarDict, Acc ++ [Term]);
variabilise_list([], _VarDict, Acc) ->
  Acc.


