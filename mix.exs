defmodule Bbs.MixProject do
  use Mix.Project

  def project do
    [
      app: :bbs,
      version: "0.1.0",
      language: :erlang,
      erlc_options:
        [
          :debug_info,
          # :warnings_as_errors,
          :warn_export_vars,
          :warn_shadow_vars,
          :warn_obsolete_guard,
          parse_transform: :lager_transform
        ] ++ erlc_options(Mix.env()),
      deps: deps()
      ]
  end

  defp erlc_options(:test), do: [:nowarn_export_all]
  defp erlc_options(_), do: []

  def application do
    [
      extra_applications: [:logger],
      mod: {:bbs_app, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:lager, ">= 0.0.0"},
      {:swarm, "~> 3.4"},
      {:gproc, git: "https://github.com/uwiger/gproc.git", branch: "uw-locks_leader"},
      {:locks, git: "https://github.com/uwiger/locks.git", override: true},
      {:zuuid, git: "https://gitlab.com/zxq9/zuuid.git", branch: "master", app: false},
      {:erlog, git: "git@bitbucket.org:netboz/erlog_bbs.git", branch: "master"}
      # {:getopt, "1.0.1", [env: :prod, repo: "hexpm",  manager: :rebar, hex: "getopt", override: true]},
      #   {:emqx, git: "https://github.com/emqx/emqx.git",
      #     branch: "master", manager: :rebar3, override: true},
      #   {:pbkdf2, git: "https://github.com/emqx/erlang-pbkdf2.git",
      #    branch: "master", override: true},
      # {:poolboy, git: "https://github.com/devinus/poolboy.git",
      #  branch: "master", override: true},
      # {:gun, [env: :prod, override: true, git: "https://github.com/emqx/gun", tag: "1.3.5"]},
      # {:cowlib, "2.8.0", [env: :prod, repo: "hexpm", hex: "cowlib", override: true]},
      # {:cuttlefish, "~> 3.0", override: true, manager: :rebar3},
      # {:goldrush, ~r/.*/, github: "basho/goldrush", tag: "0.1.9", manager: :rebar3, override: true}
    ]
  end
end