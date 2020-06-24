defmodule Todo.Server do
  @moduledoc """
  Allows multiple clients to work on a single to-do list.
  """

  # restart: :temporary means:
  # the child process is never restarted, regardless of the supervision strategy: any
  # termination (even abnormal) is considered successful.
  #
  # We do this because servers are started on demand, so if the server process isn't running, it will be started.
  # If a server crashes, it will be started on next use, so there's no need to restart it automatically.
  use GenServer, restart: :temporary

  require Logger

  def start_link(name) do
    Logger.info("Starting to-do server.")
    GenServer.start_link(__MODULE__, name, name: global_name(name))
  end

  def whereis(name) do
    case :global.whereis_name({__MODULE__, name}) do
      :undefined -> nil
      pid -> pid
    end
  end

  defp global_name(name) do
    {:global, {__MODULE__, name}}
  end

  @impl GenServer
  def init(name) do
    {:ok, {name, Todo.Database.get(name) || Todo.List.new()}}
  end

  def add_entry(todo_server, new_entry) do
    GenServer.cast(todo_server, {:add_entry, new_entry})
  end

  def entries(todo_server, date) do
    GenServer.call(todo_server, {:entries, date})
  end

  @impl GenServer
  def handle_call({:entries, date}, _from, {name, todo_list}) do
    {:reply, Todo.List.entries(todo_list, date), {name, todo_list}}
  end

  @impl GenServer
  def handle_cast({:add_entry, entry}, {name, todo_list}) do
    new_list = Todo.List.add_entry(todo_list, entry)

    Todo.Database.store(name, new_list)

    {:noreply, {name, new_list}}
  end
end
