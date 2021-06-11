# Bubble soap

A multi-agents asynchronous simulation system

## Build

    $ rebar3 compile

## Concepts


- A Bubble hosts Agents 
  

- An agent act upon Ontologies


- Agents are Bubbles


## Application start

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

These will then be available to the Mother bubble and its children.

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

Considering this defintion, our use of thr term 'Ontology' might be slighly pretencious, but this is what we are aiming to.

To make it simple, in BBS, an ontology holds knowledge : Knowdledge of swamp flowers, knowledge about how to walk on two feet, 
knowledge on how to send an http REST request...

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

"bbs:agent:mts:transport:mqtt", 
"bbs:agent:mts:transport:http"  : Two ontologies about message transport
```


## Messaging

### Introduction

Agents can exchange messages between themselves, and with the world external to their bubble ( other bubbles, 
mqtt clients, http clients...).

```"bbs:agent"``` ontology contains the needed predicates to send and receive messages.

The predicates in ```bbs:agent``` ontology are relying on some subservice ontologies to perform message transport. 
These transport ontologies are registered under ```"bbs:agent:mts``` namespace (ex : ```"bbs:agent:mts:mqtt:client"```).

### Messaging with "bbs:agent"

#### sending messages

Sending messages is made using the predicate :

```bbs:agent::goal(message_sent(Communication_Channel, To, Ontology, Message))```.

Where Message is :

```message(To, Ontology, Predicate)```

#### Receiving messages

To reaceive messages an agent needs to create a ```communication channel```



