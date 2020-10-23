#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule ExampleBasedSortTest do
  use ExUnit.Case

  test "Enum.sort/1 sorts lists" do
    assert Enum.sort([]) == []
    assert Enum.sort([1, 2, 3]) == [1, 2, 3]
    assert Enum.sort([2, 1, 3]) == [1, 2, 3]
  end
end
