#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule ListSortSmokeTest do
  use ExUnit.Case
  use ExUnitProperties

  property "quicksort/1 returns a list or fails with FunctionClauseError" do
    check all term <- term() do
      try do
        ListSort.quicksort(term)
      rescue
        FunctionClauseError ->
          :ok

        other ->
          raise "raised unexpected exception: #{inspect(other)}"
      else
        term ->
          assert is_list(term)
      end
    end
  end
end
