#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule RollingAverageServerTest do
  use ExUnit.Case

  test "some test" do
    assert {:ok, _pid} =
             RollingAverageServer.start_link(
               name: :my_server,
               max_measurements: 3
             )

    # assertions
  end

  test "some other test" do
    assert {:ok, _pid} =
             RollingAverageServer.start_link(
               name: :my_server,
               max_measurements: 3
             )

    # other assertions
  end
end
