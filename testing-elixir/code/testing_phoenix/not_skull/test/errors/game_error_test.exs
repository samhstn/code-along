#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Errors.GameErrorTest do
  use ExUnit.Case

  alias NotSkull.Errors.GameError

  describe "exception/1" do
    test "success for an empty message" do
      error = GameError.exception()

      assert %NotSkull.Errors.GameError{
               message: "Something went wrong with the game."
             } == error
    end

    test "success with message passed" do
      message = "I'm a custom message"
      error = GameError.exception(message: message)

      assert %NotSkull.Errors.GameError{
               message: message
             } == error
    end
  end
end
