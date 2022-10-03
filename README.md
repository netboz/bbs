# Bubble soap

A multi-agents asynchronous simulation system

## Concepts

- A Bubble hosts Agents 
  

- An agent act upon Ontologies


- Agents are Bubbles

## Features

* Fully asyncrhonous prolog agents made in erlang/OTP, thanks to Erlog prolog Stack.
* Clusturisable with Swarm, enabling automatic node detection, and faillure recovery. (Currently under work)
* Multiple communication protocols ( MQTT, HTTP...) (Currently under work)
* Multiple database backend support for ontologies ( Mnesia, Redis, Ets ...) (Currently under work)
* Agents can move from one context ( Bubble) to another one. (Currently under work)

## Build

    $ make build

## Run


!! current messaging protocol for agents is currently based on MQTT. Hence you need a local mqtt server running, with anonymous access allowed at the moment ( sic ) !!

To quickly run a broker locally ( be carefull about security concerns, we disclaim any dommage ) :
```
 sudo docker run -d --name emqx -p 18083:18083 -p 1883:1883 emqx/emqx:latest
```

To have a quick try locally you can use this command :

    $ make shell

Build and Run configuration will be provided shortly.

## Application start

Purpose of this section is to get a first contact with the system by studying its startup procedure.

### System ontologies registration

The goal of the application starting process is to have the root bubble running. 

The root bubble can spawn other agents/bubbles, who may themselves spawn some other agents/bubbles.
This leads to a bubble tree architecture.

For the root bubble to be able to spawn childs, and also for the bubbles to be a pleasant place to live for agents
a set of system services needs to be provided ( messaging, ontology manipulation, etc).

These services are the ```System Ontologies``` and are written partly in Prolog, partly in Erlang.

They are located in 

- Prolog sources : 

  ```shell
    $ priv/ontologies
  ``` 

- erlang sources :

```shell
      $ src/ontologies
```

At application start, these ```system ontologies``` will have their prolog code validated, and 
the presence of the corresponding erlang module will be checked. 

They are defined in application ```app.config```

```erlang
  {bbs, [
        {ontologies, [
          {<<"bbs:root">>, [{file, "ont_bbs_root.pl"}], []},
          {<<"bbs:bubble">>, [{file, "ont_bbs_bubble.pl"}], [ont_bbs_bubble]},
          {<<"bbs:brain_tests">>, [{file, "ont_bbs_brain_test.pl"}], []},
          {<<"bbs:agent">>, [{file, "ont_bbs_agent.pl"}], [ont_bbs_agent]},
          {<<"bbs:mts:mqtt:broker">>, [{file, "ont_bbs_mts_mqtt_broker.pl"}], [ont_bbs_mts_mqtt_broker]},
          {<<"bbs:mts:mqtt:client">>, [{file, "ont_bbs_mts_client_gproc.pl"}], [ont_bbs_mts_mqtt_client]}
        ]}
        ]
    }
```

The format is :

```erlang
{OntologieName, ListOfPrologFiles, ListOfErlangModule}
```

### Root bubble startup

To complete application startup, the root bubble is started.

In details, this means the root bubble agent will be spawned. It will then initialize a defined set of ontologies among 
the ones registered in previous steps.

This starting process is the same for all agents running on the platform.

The default ontologies initialized started by the root bubble are : (They are described in details further on)

```erlang
[
    %% Mandatory Ontology fpr all bbs agents
    {ontology, <<"bbs:agent">>, [], bbs_db_ets},
    %% Makes this agent the mother bubble
    {ontology, <<"bbs:root">>, [], bbs_db_ets},
    %% This ontology turns the agent into bubble
    {ontology, <<"bbs:bubble">>, [], bbs_db_ets},
    %% Root bubble start the mqtt broker service
    {ontology, <<"bbs:mts:mqtt:broker">>, [], bbs_db_ets},
    %% gives mother bubble mqtt communication ability
    {ontology, <<"bbs:mts:mqtt:client">>, [], bbs_db_ets}
  ].
```

Where each entry is :

```erlang
{ontology, OntologyNamespace, OntologyParameters, OntologyStorageMethod}
```

## Ontologies

### Introduction

Quoting Wikipedia :

```text
In computer science and information science, an ontology encompasses a representation, formal naming and definition of 
the categories, properties and relations between the concepts, data and entities that substantiate one, many, or all 
domains of discourse.
```

Considering this definition, our use of thr term 'Ontology' might be slighly pretencious, but this is what we are aiming to.

To make it simple, in BBS, an ontology holds knowledge : Knowdledge of swamp flowers, knowledge of trapist beers, 
knowledge about how to walk on two feet, optionally, knowledge on how to send an http REST request...

The agents can use these ontologies to perform tasks, answer questions related to a domain.

In BBS, ontologies are written mainly in Prolog ( and erlang, for some low level system predicates )

Each Ontology is identified by a namespace.

### Ontology namespaces

Each ontology is identified by a namespace which is a string whose format is :

```text
bbs:subdomain1:subdomain2...:subdomain N
```

Exemple :

```text
"bbs:agent" : Containing system ontology for BBS agents

"bbs:mts:client:mqtt", 
"bbs:mts:client:gproc"  : Two ontologies about messages transport
```

### Actions

A commonly used design pattern, in bbs ontologies, are the ```Actions```

Let's start with an exemple :

```prolog
action(drink_bottle(Bottle), [cap_opened(Bottle)], empty(Bottle)).
```

This naive action predicates says : To reach a final state where a ```Bottle``` is empty, if the bottle is not already empty,
a solution is to have the cap open, and, then drink it.

Practically, it means if you send to your agent the predicate : ```goal(empty(bottle_of_beer))```,
It will check if ```bottle_of_beer is empty```. if not, it will try to satisfy subgoals. 
Here, one sub-goal: ```cap_opened(Bottle).```. So, the agent will try to prove : ```goal(cap_opened(bottle_of_beer))``` first
and if it succeed, as they are no more pre-requisite, drink it.

Advice : don't let your boose hanging around.

Please note that there might be multiple way to empty a bottle, this can easily be described, by adding new ```Action``` 
clauses leading to same final state. Here we tell our agent another solution is to have cap open, put bottle head down, and wait and hour.
This might not be optimal, but should work....if it would come to fail, then the next action leading to this final state would be tried.

```prolog
action(drink_bottle(Bottle), [cap_opened(Bottle)], empty(Bottle)).
action(wait(hour(1)), [cap_opened(Bottle), upside_down(Bottle)], empty(Bottle)).
```

In a more academic format, Actions are taking this form :

````prolog
action(Transition_Predicate_1, [List_of_prequesites_or_original_states_for_Transition_predicate_1], FinalState).
action(Transisition_Predicate_2,[List_of_prequesites_or_original_states_for_Transition_predicate_2], FinalState).
````

Actions are used in conjonction with ```goal/1``` predicate.

```prolog
goal(FinalState).
```

```Goal``` predicate will try to match its single parameter
with one of the final states defined among the actions of the current ontology.

ex:
```prolog
goal(empty(bottle_of_jenlin)).
```

## Messaging

### Introduction

Agents can exchange messages between themselves, and with the world external to their bubble ( other bubbles, 
mqtt clients, http clients...).

```"bbs:agent"``` ontology contains the needed predicates to send and receive messages.

Note. The action predicates in ```bbs:agent``` ontology are relying on some sub-ontologies to perform message transport. 
These transport ontologies are registered under ```"bbs:mts``` namespace (ex : ```"bbs:mts:client:mqtt"```).

### Messaging with "bbs:agent"

#### sending messages

Sending messages is made using the ```message_sent/4``` action predicate :

Ex: 

```prolog
bbs:agent::goal(message_sent(Communication_Channel, To, Ontology, Message)).
```

Where Message is the payload effectively sent. It can be any prolog valid term, from a simple string or integer to more complex 
nested predicates.

```Message``` parameter is the only variable that needs to be binded.

Hence if you ask : 

```prolog
bbs:agent::goal(message_sent(Communication_Channel, To, Ontology, "I am a drunkyard"))
```

As none of Communication_Channel, To, Ontology are binded, the message will be sent to the first contact found into agent addressbook
After all, this is what you asked : please have this message sent ( no matter to whom).

#### Receiving messages

To reaceive messages an agent needs to create a ```communication channel```



