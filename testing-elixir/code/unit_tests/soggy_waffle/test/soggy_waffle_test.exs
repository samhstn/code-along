defmodule SoggyWaffleTest do
  use ExUnit.Case, async: true

  describe "rain?/2" do
    test "success: gets forecasts, returns true for imminent rain" do
      now = DateTime.utc_now()
      future_unix = DateTime.to_unix(now) + 1
      expected_city = Enum.random(["Denver", "Los Angeles", "New York"])

      weather_fn_double = fn city ->
        send(self(), {:get_forecast_called, city})
        drizzle_id = 300
        data = [%{"dt" => future_unix, "weather" => [%{"id" => drizzle_id}]}]
        {:ok, %{"list" => data}}
      end

      assert SoggyWaffle.rain?(expected_city, now, weather_fn_double)

      assert_received(
        {:get_forecast_called, ^expected_city},
        "get_forecast was never called"
      )
    end
  end
end
