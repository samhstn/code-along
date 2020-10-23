#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule GenServerThatUsesFile do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def store(pid, term) do
    GenServer.cast(pid, {:store, term})
  end

  @impl true
  def init(opts) do
    path = Keyword.fetch!(opts, :path)

    File.touch!(path)

    pid = self()
    ref = make_ref()
    spawn_link(fn -> monitor_for_cleanup(pid, ref, path) end)

    # Wait for the cleanup process to be ready, so that if this process
    # crashes before the cleanup process is trapping exits then we don't
    # leave a zombie process.
    receive do
      {^ref, :ready} -> :ok
    end

    {:ok, path}
  end

  @impl true
  def handle_cast({:store, term}, path) do
    new_content = "\n" <> :erlang.term_to_binary(term)
    File.write!(path, new_content, [:binary, :append])
    {:noreply, path}
  end

  defp monitor_for_cleanup(pid, ref, path) do
    Process.flag(:trap_exit, true)
    send(pid, {ref, :ready})

    receive do
      {:EXIT, ^pid, _reason} ->
        File.rm_rf!(path)
    end
  end
end
