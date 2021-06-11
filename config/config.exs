# TODO: replace with Config when we will migrate to Elixir 1.9
use Mix.Config

config_file = "app.config"

{:ok, [sys_config]} = :file.consult(Path.join('config', config_file))
for {app, config} <- sys_config, do: config(app, config)
