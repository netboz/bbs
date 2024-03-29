
import Config


config  :bbs,
  ontologies: [
    {"bbs:root", [{:file, "ont_bbs_root.pl"}], []},
    {"bbs:agent", [{:file, "ont_bbs_agent.pl"}], [:ont_bbs_agent]},
    {"bbs:bubble", [{:file, "ont_bbs_bubble.pl"}], [:ont_bbs_bubble]},
    {"bbs:brain_tests", [{:file, "ont_bbs_brain_test.pl"}], []},
    {"bbs:brain_tests:data", [{:file, "ont_bbs_brain_test_data.pl"}], []},
    {"bbs:mts:client:registry", [{:file, "ont_bbs_mts_client_registry.pl"}], [:ont_bbs_mts_client_registry]},
    {"bbs:mts:client:mqtt", [{:file, "ont_bbs_mts_client_mqtt.pl"}], [:ont_bbs_mts_client_mqtt]}
    #{"bbs:mts:client:gproc", [{:file, "ont_bbs_mts_client_gproc.pl"}], [:ont_bbs_mts_client_gproc]},
  ],
  databases_backends: [
    {:bbs_db_ets},
    {:bbs_db_dict},
    {:bbs_db_redis, []}
  ]

config  :lager,
  colored: :true,
  handlers:  [
    {:lager_console_backend, [{:level, :debug}, {:formatter, :lager_default_formatter},
      {:formatter_config, [:time," [",:severity,"] ",:pid, " ", :message, "\n"]}]},
    {:lager_file_backend, [{:file, "error.log"}, {:level, :error}, {:formatter, :lager_default_formatter},
      {:formatter_config, [:date, " ", :time," [",:severity,"] ",:pid, " ", :message, "\n"]}]},
    {:lager_file_backend, [{:file, "console.log"}, {:level, :info}]}
  ]
