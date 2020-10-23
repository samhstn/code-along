#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.WeatherChecker do
  use GenServer

  # same module attribute and start_link/1 as before

  @impl GenServer
  def init(opts) do
    mode = Keyword.get(opts, :mode, :periodic)
    interval = Keyword.fetch!(opts, :interval)

    state = %{
      city: Keyword.fetch!(opts, :city),
      phone_number: Keyword.fetch!(opts, :phone_number),
    }

    case mode do
      :periodic ->
        :timer.send_interval(interval, self(), :tick)

      :manual ->
        :ok
    end

    {:ok, state}
  end

  @impl GenServer
  def handle_info(:tick, state) do
    # exactly the same code as before
  end
end
