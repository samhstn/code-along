#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule RandomizedSortTest do
  use ExUnit.Case

  test "Enum.sort/1 sorts lists" do
    for _ <- 1..10 do                                   
      random_list = random_list()
      sorted_list = Enum.sort(random_list)

      assert length(random_list) == length(sorted_list) 
      assert sorted?(sorted_list)                       
    end
  end

  defp random_list do
    Stream.repeatedly(fn -> Enum.random(-100..100) end)
    |> Enum.take(_length = Enum.random(0..10))
  end

  defp sorted?([first, second | rest]),
    do: first <= second and sorted?([second | rest])

  defp sorted?(_other), do: true
end
