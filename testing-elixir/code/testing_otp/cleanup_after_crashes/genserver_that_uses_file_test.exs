#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule GenServerThatUsesFileTest do
  use ExUnit.Case

  test "no file is left behind if the GenServer process crashes" do
    path =
      Path.join(
        System.tmp_dir!(),
        Integer.to_string(System.unique_integer([:positive]))
      )

    pid = start_supervised!({GenServerThatUsesFile, path: path})

    assert File.exists?(path)

    Process.exit(pid, :kill)

    wait_for_passing(_2_seconds = 2000, fn ->
      refute File.exists?(path)
    end)
  end


  defp wait_for_passing(timeout, fun) when timeout > 0 do
    fun.()
  rescue
    _ ->
      Process.sleep(100)
      wait_for_passing(timeout - 100, fun)
  end

  defp wait_for_passing(_timeout, fun), do: fun.()
end
