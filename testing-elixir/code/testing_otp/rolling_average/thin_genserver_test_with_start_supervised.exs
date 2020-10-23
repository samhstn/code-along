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

  describe "initialization" do
    test "accepts a measurement count on start" do
      assert {:ok, _pid} =
               start_supervised(
                 {RollingAverageServer, max_measurements: 3}
               )
    end
  end
  
  describe "adding and averaging" do
    test "it returns the rolling average for the elements" do
      pid = start_supervised!({RollingAverageServer, max_measurements: 2})

      RollingAverageServer.add_element(pid, 5)
      RollingAverageServer.add_element(pid, 6)
      
      assert RollingAverageServer.average(pid) == 5.5
      
      RollingAverageServer.add_element(pid, 7)

      assert RollingAverageServer.average(pid) == 6.5
    end
  end
  
  describe "error cases" do
    test "asking for the average if the server has stopped crashes" do
      pid = start_supervised!({RollingAverageServer, max_measurements: 2})
      
      RollingAverageServer.add_element(pid, 5)
      RollingAverageServer.add_element(pid, 6)
      
      stop_supervised!(RollingAverageServer)
      
      assert {:noproc, _} = catch_exit(RollingAverageServer.average(pid))
    end
  end
end
