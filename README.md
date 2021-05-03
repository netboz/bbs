# Bubble soap

A multi-agents asynchronous simulation system

## Build

    $ rebar3 compile

## Concepts


- A Bubble hosts Agents 
  

- An agent is an autonomous actor whose behaviour is based on upon Ontologies


- Agents are Bubbles


- The root bubble is called the mother bubble.

## Application start

###1 System ontologies registration

The goal of the application starting process is to have the mother bubble running. 

The mother bubble will spawn some agents/bubbles, who may themselves spawn some other agents/bubbles
leading to a bubble tree architecture.

For the mother bubble to be able to spawn childs, and also for the bubbles to be a pleasant place to live for agents
a set of system services needs to be provided ( messaging, ontology manipulation, etc).

These services are the ```System Ontologies``` and are written partly in Prolog, partly in Erlang for the side effects.

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
          {<<"bbs:mts:mqtt:client">>, [{file, "ont_bbs_mts_mqtt_client.pl"}], [ont_bbs_mts_mqtt_client]}
        ]}
        ]
    }
```

The format is :

```erlang
{OntologieName, ListOfPrologFiles, ListOfErlangModule}
```


These will then be available to the Mother bubble and its children.

###2 Mother bubble startup

To complete application startup, the mother bubble is started.

In details, this means the mother bubble agent will spawned. It will then initialize a defined set of ontologies among 
the ones registered in previous steps.

This starting process is the same for all agents running on the platform.

The default ontologies initialized started by the mother bubble are : (They are described in details further on)

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










