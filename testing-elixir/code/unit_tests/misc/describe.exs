#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule YourApp.YourModuleTest do
  use ExUnit.Case

  describe "thing_to_do/1" do
    test "it returns :ok, calls the function if the key is correct"
    test "it does not call the function if the key is wrong"
  end
end
