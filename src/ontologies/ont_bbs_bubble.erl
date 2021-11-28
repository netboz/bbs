%%%-------------------------------------------------------------------
%%% @author yan
%%% @copyright (C) 2021, QengHo
%%% @doc
%%%
%%% @end
%%% Created : 07. Jan 2021 8:29 PM
%%%-------------------------------------------------------------------

-module(ont_bbs_bubble).

-author("yan").

-include("bbs.hrl").
-include("utils.hrl").

-include_lib("erlog/include/erlog_int.hrl").

-export([external_predicates/0]).
-export([register_bubble_predicate/3, spawn_child_predicate/3, stop_child_predicate/3,
         terminate_child_predicate/3, child_predicate/3]).

%% Prolog API
-define(ERLANG_PREDS,
        [{{register_bubble, 1}, ?MODULE, register_bubble_predicate},
         {{spawn_child, 1}, ?MODULE, spawn_child_predicate},
         {{stop_child, 1}, ?MODULE, stop_child_predicate},
         {{terminate_child, 1}, ?MODULE, terminate_child_predicate},
         {{child, 1}, ?MODULE, child_predicate}]).

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% Return the list of built in predicates contained in this module
%%
%% @end
%%------------------------------------------------------------------------------

external_predicates() ->
    ?ERLANG_PREDS.

register_bubble_predicate({_Atom, NodeName}, Next0, #est{bs = Bs} = St) ->
    DNodeName =
        case erlog_int:dderef(NodeName, Bs) of
            {_} ->
                uuid:uuid4();
            _ ->
                NodeName
        end,
    case ?HORDEREG:update_value(?BBS_BUBBLES_REG,
                                {reg, get(tree_node), get(agent_name)},
                                fun(_oldval) -> DNodeName end)
    of
        {DNodeName, _} ->
            put(children_node, DNodeName),
            erlog_int:unify_prove_body(DNodeName, NodeName, Next0, St);
        Other ->
            ?WARNING_MSG("Failled to register bubble :~p", [Other]),
            erlog_int:fail(St)
    end.

%% @doc spawn a child locally on the AP. AgSpecs contains Agent descripion
spawn_child_predicate({_Atom, {agent, Name, Onts}}, Next0, #est{} = St) ->
    Me = get(agent_name),
    ChildrenNode = get(children_node),
    AgentSpecs =
        #agent{name = Name,
               parent = Me,
               startup_ontologies = Onts},
    case ?HORDESUP:start_child(?BBS_BUBLES_SUP, child_specs(AgentSpecs, ChildrenNode)) of
        {ok, Pid} ->
            erlang:monitor(process, Pid, [{tag, {'DOWN', Name}}]),
            erlog_int:prove_body(Next0, St);
        {error, Reason} ->
            ?ERROR_MSG("Failled to create agent :~p Reason :~p", [Name, Reason]),
            erlog_int:fail(St)
    end.

%% @doc Gracefully ask a local child to stop.
stop_child_predicate({_Atom, Name}, Next0, #est{bs = Bs} = St) ->
    DName = erlog_int:dderef(Name, Bs),
    ChildrenNode = get(children_node),
    ?INFO_MSG("DNAME ~p", [DName]),
    ?INFO_MSG("DUM", [?HORDEREG:select(?BBS_BUBBLES_REG, [{{'$1', '_', '_'}, [], ['$1']}])]),
    case get_child_pid(ChildrenNode, DName) of
        undefined ->
            ?WARNING_MSG("Process not found ~p", [{ChildrenNode, DName}]),
            erlog_int:fail(St);
        Pid ->
            case erlang:is_process_alive(Pid) of
                true ->
                    exit(Pid, normal),
                    erlog_int:prove_body(Next0, St);
                _ ->
                    erlog_int:fail(St)
            end
    end.

%% @doc Brutally terminate child process
terminate_child_predicate({_Atom, Name}, Next0, #est{bs = Bs} = St) ->
    DName = erlog_int:dderef(Name, Bs),
    ChildrenNode = get(children_node),
    ?INFO_MSG("DNAME ~p", [DName]),
    case get_child_pid(ChildrenNode, DName) of
        undefined ->
            ?WARNING_MSG("Process not found ~p", [{ChildrenNode, DName}]),
            erlog_int:fail(St);
        Pid ->
            case erlang:is_process_alive(Pid) of
                true ->
                    exit(Pid, kill),
                    erlog_int:prove_body(Next0, St);
                _ ->
                    erlog_int:fail(St)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @private
%% True if agent exits
%%
%% @end
%%------------------------------------------------------------------------------

child_predicate({_Atom, AgentName}, Next0, #est{bs = Bs} = St) ->
    ?INFO_MSG("Child predicate", []),
    DAgentName = erlog_int:dderef(AgentName, Bs),
    do_child_predicate(DAgentName, Next0, St).

do_child_predicate(BinAgentName, Next0, #est{} = St) when is_binary(BinAgentName) ->
    case ?HORDEREG:lookup(?BBS_BUBBLES_REG, {get(children_node), BinAgentName}) of
        [] ->
            erlog_int:fail(St);
        _ ->
            erlog_int:prove_body(Next0, St)
    end;
do_child_predicate(DAgentName, Next0, #est{} = St) ->
    SelectedAgents = ?HORDEREG:select(?BBS_BUBBLES_REG, [{{{get(children_node), '$1'}, '_', '_'}, [], ['$1']}]),
    ?INFO_MSG("SelectedAgents ~p", [SelectedAgents]),
    do_child_predicate2(DAgentName, Next0, St, SelectedAgents).

do_child_predicate2(_DAgentName, _Next0, St, []) ->
    erlog_int:fail(St);
do_child_predicate2(DAgentName,
                    Next0,
                    #est{bs = Bs,
                         vn = Vn,
                         cps = Cps} =
                        St,
                    [Ragent | Tail]) ->
    case erlog_int:unify(DAgentName, Ragent, Bs) of
        {succeed, NewBs} ->
            ?INFO_MSG("Unified Agent", []),

            FailFun =
                fun(#cp{next = NextF,
                        bs = Bs0,
                        vn = Vnf},
                    LCps,
                    Lst) ->
                   do_child_predicate2(DAgentName,
                                       NextF,
                                       Lst#est{cps = LCps,
                                               bs = Bs0,
                                               vn = Vnf},
                                       Tail)
                end,
            Cp = #cp{type = compiled,
                     data = FailFun,
                     next = Next0,
                     bs = Bs,
                     vn = Vn},
            erlog_int:prove_body(Next0, St#est{bs = NewBs, cps = [Cp | Cps]});
        _ ->
            ?INFO_MSG("NOT unified", []),
            erlog_int:fail(St)
    end.

%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%

child_specs(ChildParameters, TreeNode) ->
    Id = ChildParameters#agent.name,
    #{id => Id,
      start => {bbs_agent, start_link, [ChildParameters, TreeNode]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [bbs_agent_sup]}.

get_child_pid(AgentNode, AgentName) ->
    case ?HORDEREG:lookup(?BBS_BUBBLES_REG, {AgentNode, AgentName}) of
        [{Pid, _}] ->
            Pid;
        _ ->
            undefined
    end.
