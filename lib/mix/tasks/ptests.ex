defmodule Mix.Tasks.Ptests do
  @moduledoc "Run bbs prolog ontology test suites"
  @shortdoc "Echoes arguments"

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    Mix.shell().info(Enum.join(args, " "))
  end
end