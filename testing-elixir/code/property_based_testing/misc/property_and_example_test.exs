#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule StringTest do
  use ExUnit.Case
  use ExUnitProperties

  describe "String.contains?/2" do
    property "concatenation of a and b contains both" do
      check all left <- string(),
                right <- string(),
                concatenated = left <> right do
        assert String.contains?(concatenate, left)
        assert String.contains?(concatenate, right)
      end
    end

    test "String.contains?/2 works on known inputs" do
      assert String.contains?("foobar", "foo")
      assert String.contains?("foobar", "bar")
      assert String.contains?("foobar", "ob")
      refute String.contains?("foobar", "baz")
    end
  end
end
