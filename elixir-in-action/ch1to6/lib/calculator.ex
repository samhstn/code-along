defmodule Calculator do
  def start(initial_state \\ 0) do
    spawn(fn -> loop(initial_state) end)
  end

  defp process_operation(pid, operation) do
    to_send =
      case operation do
        :value -> {:value, self()}
        {:add, n} -> {:add, self(), n}
        {:sub, n} -> {:sub, self(), n}
        {:mul, n} -> {:mul, self(), n}
        {:div, n} -> {:div, self(), n}
      end

    send(pid, to_send)

    receive do
      {:result, result} -> result
      {:error, error} -> raise ArithmeticError, error
    after
      5000 -> {:error, :timeout}
    end
  end

  def value(pid), do: process_operation(pid, :value)
  def add(pid, n), do: process_operation(pid, {:add, n})
  def sub(pid, n), do: process_operation(pid, {:sub, n})
  def div(pid, n), do: process_operation(pid, {:div, n})
  def mul(pid, n), do: process_operation(pid, {:mul, n})

  defp loop(state) do
    receive do
      {:value, caller} ->
        send(caller, {:result, state})
        loop(state)
      {:add, caller, n} ->
        send(caller, {:result, state + n})
        loop(state + n)
      {:sub, caller, n} ->
        send(caller, {:result, state - n})
        loop(state - n)
      {:mul, caller, n} ->
        send(caller, {:result, state * n})
        loop(state * n)
      {:div, caller, 0} ->
        send(caller, {:error, "can't divide by 0"})
        loop(state)
      {:div, caller, n} ->
        send(caller, {:result, state / n})
        loop(state / n)
    end
  end
end
