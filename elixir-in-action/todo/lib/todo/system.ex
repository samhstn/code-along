defmodule Todo.System do
  @moduledoc """
  System can be started with 

  iex>Todo.System.start_link
  """
  require Logger

  def start_link do
    Logger.info("Starting to-do system.")

    Supervisor.start_link(
      [
        Todo.Database,
        Todo.Cache,
        Todo.Web
      ],
      strategy: :one_for_one
    )
  end
end
