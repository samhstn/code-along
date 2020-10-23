#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule ListSortTest do
  use ExUnit.Case
  use ExUnitProperties

  property "quicksort/1 correctly sorts lists" do
    check all list <- list_of(term()) do
      assert ListSort.quicksort(list) == :lists.sort(list)
    end
  end
end
