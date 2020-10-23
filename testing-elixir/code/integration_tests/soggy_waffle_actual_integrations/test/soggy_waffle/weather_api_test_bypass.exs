#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.WeatherAPITest do
  use ExUnit.Case

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  test "get_forecast/1 hits GET /data/2.5/forecast", %{bypass: bypass} do
    query = "losangeles"
    app_id = "MY_APP_ID"
    test_server_url = "http://localhost:4040"
    
    forecast_data = %{
      "list" => [
        %{
          "dt" => DateTime.to_unix(DateTime.utc_now()) + _seconds = 60,
          "weather" => [%{"id" => _thunderstorm = 231}]
        }
      ]
    }

    Bypass.expect_once(bypass, "GET", "/data/2.5/forecast", fn conn ->
      conn = Plug.Conn.fetch_query_params(conn)

      assert conn.query_params["q"] == query
      assert conn.query_params["APPID"] == app_id

      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.resp(200, Jason.encode!(forecast_data))
    end)

    assert {:ok, body} =
             SoggyWaffle.WeatherAPI.get_forecast(
               "Los Angeles",
               test_server_url
             )

    assert body == forecast_data
  end
end
