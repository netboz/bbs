# TODO: replace with Config when we will migrate to Elixir 1.9
use Mix.Config

config_file = "app.config"

case Mix.env do
  # Config used in development environment
  :test          -> config_file = dev.config
  # Configuration used in production
  :prod_         -> config_file = prod.config
  # Configuration used in bitbucket pipeline
  :pip           -> config_file = pip.config
  # Default configuration : test
  _              -> config_file = dev.config
end

{:ok, [sys_config]} = :file.consult(Path.join('config', config_file))
for {app, config} <- sys_config, do: config(app, config)
