#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle do
  alias SoggyWaffle.Weather

  @thunderstorm_ids [200, 201, 202, 210, 211, 212, 221, 230, 231, 232]
  @drizzle_ids [300, 301, 302, 310, 311, 312, 313, 314, 321]
  @rain_ids [500, 501, 502, 503, 504, 511, 520, 521, 522, 531]
  @all_rain_ids @thunderstorm_ids ++ @drizzle_ids ++ @rain_ids

  def rain?(city) do
    with {:ok, response} <- SoggyWaffle.WeatherAPI.get_forecast(city) do
      weather_data = parse_response(response)

      SoggyWaffle.Weather.imminent_rain?(weather_data)
    end
  end

  defp parse_response(response) do
    # parsing logic
  end
end
