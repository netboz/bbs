# TODO: replace with Config when we will migrate to Elixir 1.9
use Mix.Config

config_file = case Mix.env do
  # Config used in development environment
  :test          -> "dev.config"
  # Configuration used in production
  :prod_         -> "prod.config"
  # Configuration used in bitbucket pipeline
  :pip           -> "pip.config"
  # Default configuration : test
  _              -> "dev.config"
end

inspect("Using configuration :#{inspect(config_file)}")
{:ok, [sys_config]} = :file.consult(Path.join('config', config_file))
for {app, config} <- sys_config, do: config(app, config)
