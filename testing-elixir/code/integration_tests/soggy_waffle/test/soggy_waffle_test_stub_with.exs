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

  setup :set_mox_global
  setup :verify_on_exit!

  describe "rain?/2" do
    test "success: using the real API" do
      Mox.stub_with(SoggyWaffle.WeatherAPIMock, SoggyWaffle.WeatherAPI)

      # rest of the test
    end
  end
end
