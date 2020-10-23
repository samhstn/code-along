#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule SoggyWaffle.WeatherTest do
  use ExUnit.Case
  alias SoggyWaffle.Weather

  describe "imminent_rain?/2" do
    test "returns true when it will rain in the future" do
      now = datetime(hour: 0, minute: 0, second: 0) 
      one_second_from_now = datetime(hour: 0, minute: 0, second: 1)  

      weather_data = [weather_struct(one_second_from_now, :rain)]

      assert Weather.imminent_rain?(weather_data, now) == true
    end
  end

  defp weather_struct(datetime, condition) do
    %Weather{
      datetime: datetime,
      rain?: condition == :rain
    }
  end

  defp datetime(options) do
    %DateTime{
      calendar: Calendar.ISO,
      day: 1,
      hour: Keyword.fetch!(options, :hour),
      microsecond: {0, 0},
      minute: Keyword.fetch!(options, :minute),
      month: 1,
      second: Keyword.fetch!(options, :second),
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 2020,
      zone_abbr: "UTC"
    }
  end
end
