#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule ListSort do
  def quicksort([head | rest]) do
    {smaller_elements, larger_elements} =
      Enum.split_with(rest, &(&1 <= head))

    quicksort(smaller_elements) ++ [head] ++ quicksort(larger_elements)
  end

  def quicksort([]), do: []
end
