#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule FirstPropertySortTest do
  use ExUnit.Case
  use ExUnitProperties

  property "Enum.sort/1 sorts lists" do
    check all list <- list_of(integer()) do
      sorted_list = Enum.sort(list)

      assert length(list) == length(sorted_list)
      assert sorted?(sorted_list)
    end
  end

  defp sorted?([first, second | rest]),
    do: first <= second and sorted?([second | rest])

  defp sorted?(_other), do: true
end
