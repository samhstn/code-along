#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffleTest do
  use ExUnit.Case

  import Mox

  setup :verify_on_exit!

  describe "rain?/2" do
    test "success: gets forecasts, returns true for imminent rain" do
      expect(SoggyWaffle.WeatherAPIMock, :get_forecast, 1, fn city ->
        assert city == "Los Angeles"

        response = %{
          "list" => [
            %{
              "dt" => DateTime.to_unix(DateTime.utc_now()) + _seconds = 60,
              "weather" => [%{"id" => _thunderstorm = 231}]
            }
          ]
        }

        {:ok, response}
      end)

      assert SoggyWaffle.rain?("Los Angeles", DateTime.utc_now())
    end
  end
end
