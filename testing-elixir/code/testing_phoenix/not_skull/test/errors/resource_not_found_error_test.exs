#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule NotSkull.Errors.ResourceNotFoundErrorTest do
  use ExUnit.Case

  alias NotSkull.Errors.ResourceNotFoundError

  describe "exception/1" do
    test "success" do
      id = Enum.random(1..1000)
      resource = Enum.random(["User", "Business profile"])

      error = ResourceNotFoundError.exception(resource: resource, id: id)

      assert 404 == error.plug_status
      assert "#{resource} not found for #{id}" == error.message
    end
  end
end
