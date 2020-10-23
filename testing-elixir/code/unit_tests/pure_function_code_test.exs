#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule YourMathModule do
  use ExUnit.Case

  describe "double/1" do
    test "it doubles a number" do
      input = 1_000_000

      response = YourMathModule.double(input)

      assert response == 2_000_000
    end

    test "it returns :error if given something other than a number" do
      bad_values = [:atom, "string", %{}, []]

      for bad_value <- bad_values do
        assert :error = YourMathModule.double(bad_value)
      end
    end
  end
end
