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

  describe "rain?/2" do
    test "success: gets forecasts, returns true for imminent rain" do
      log =
        capture_log(fn ->
          SoggyWaffle.rain?("Los Angeles", DateTime.utc_now())
        end)

      assert log =~ "Getting forecast for city: Los Angeles"
    end
  end
end
