#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule RollingAverageServer do
  use GenServer

  defstruct [:measurements]

  def start_link(options) do
    max_measurements = Keyword.fetch!(options, :max_measurements)
    GenServer.start_link(__MODULE__, max_measurements)
  end

  def add_element(pid, element) do
    GenServer.cast(pid, {:add_element, element})
  end

  def average(pid) do
    GenServer.call(pid, :average)
  end

  @impl true
  def init(max_measurements) do
    measurements = RollingAverageMeasurements.new(max_measurements)
    {:ok, %__MODULE__{measurements: measurements}}
  end

  @impl true
  def handle_call(:average, _from, state) do
    average = RollingAverageMeasurements.average(state.measurements)
    {:reply, average, state}
  end

  @impl true
  def handle_cast({:add_element, new_element}, state) do
    measurements =
      RollingAverageMeasurements.add_element(
        state.measurements,
        new_element
      )

    {:noreply, %__MODULE__{state | measurements: measurements}}
  end
end
