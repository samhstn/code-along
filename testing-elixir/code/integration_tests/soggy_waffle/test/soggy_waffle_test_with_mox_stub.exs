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
  
  import ExUnit.CaptureLog
  import Mox
  
  require Logger

  describe "rain?/2" do
    test "success: gets forecasts, returns true for imminent rain" do
      stub(SoggyWaffle.WeatherAPIMock, :get_forecast, fn city ->
        Logger.info("Getting forecast for city: #{city}")

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

      log =
        capture_log(fn ->
          assert SoggyWaffle.rain?("Los Angeles", DateTime.utc_now())
        end)

      assert log =~ "Getting forecast for city: Los Angeles"
    end
  end
end
