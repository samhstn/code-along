#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.FakeWeatherAPI do
  require Logger

  @spec get_forecast(String.t()) :: {:ok, map()}
  def get_forecast(city) do
    _ = Logger.info("Getting forecast for city: #{city}")

    response = %{
      "list" => [
        %{
          "dt" => DateTime.to_unix(DateTime.utc_now()),
          "weather" => [%{"id" => _thunderstorm = 231}]
        }
      ]
    }

    {:ok, response}
  end
end
