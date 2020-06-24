defmodule ServerProcess do
  @moduledoc """
  1. Accepts a plug-in module as the argument.
  2. Maintains the module atom as part of the process state.
  3. Invokes callback-module functions when needed.

  For this to work a callback module must implement and export a well-defined set of functions.
  """
  def start(callback_module) do
    spawn(fn ->
      initial_state = callback_module.init()
      loop(callback_module, initial_state)
    end)
  end

  defp loop(callback_module, current_state) do
    receive do
      {:call, request, caller} ->
        {response, new_state} =
          callback_module.handle_call(
            request,
            current_state
          )
        send(caller, {:response, response})
        loop(callback_module, new_state)
      {:cast, request} ->
        new_state =
          callback_module.handle_cast(
            request,
            current_state
          )
        loop(callback_module, new_state)
    end
  end

  def call(server_pid, request) do
    send(server_pid, {:call, request, self()})

    receive do
      {:response, response} ->
        response
    end
  end

  def cast(server_pid, request) do
    send(server_pid, {:cast, request})
  end
end

defmodule ServerProcessKeyValueStore do
  def start do
    ServerProcess.start(ServerProcessKeyValueStore)
  end

  def put(pid, key, value) do
    ServerProcess.cast(pid, {:put, key, value})
  end

  def get(pid, key) do
    ServerProcess.call(pid, {:get, key})
  end

  def init do
    %{}
  end

  def handle_call({:put, key, value}, state) do
    {:ok, Map.put(state, key, value)}
  end

  def handle_call({:get, key}, state) do
    {Map.get(state, key), state}
  end

  def handle_cast({:put, key, value}, state) do
    Map.put(state, key, value)
  end
end
