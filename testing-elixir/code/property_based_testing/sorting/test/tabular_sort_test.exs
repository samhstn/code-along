#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule TabularSortTest do
  use ExUnit.Case

  test "Enum.sort/1 sorts lists" do
    inputs_and_outputs = [
      {[], []},
      {[1, 2, 3], [1, 2, 3]},
      {[2, 1, 3], [1, 2, 3]},
      {[2, 1, 2], [1, 2, 2]},
      {[0, -1, -2], [-2, -1, 0]}
    ]

    for {input, expected_output} <- inputs_and_outputs do
      assert Enum.sort(input) == expected_output
    end
  end
end
