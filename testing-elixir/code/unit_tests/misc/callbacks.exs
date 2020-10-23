#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule YourApp.YourSetupTest do
  use ExUnit.Case
  
  setup_all do
    function_to_not_call = fn ->
      flunk("this function should not have been called")
    end
  
    function_to_call = fn -> send(self(), :function_called) end
  
    %{bad_function: function_to_not_call, good_function: function_to_call}
  end
  
  setup do
    function_to_not_call = fn ->
      flunk("this function should not have been called")
    end

    function_to_call = fn -> send(self(), :function_called) end

    %{bad_function: function_to_not_call, good_function: function_to_call}
  end
  
  setup do
    file_name = "example.txt"
    :ok = File.write(file_name, "hello")
  
    on_exit(fn ->
      File.rm(file_name)
    end)
  
    %{file_name: file_name}
  end

  test "does not call the function if the key is wrong",
       %{bad_function: bad_function} do
    assert {:error, _} =
             YourModule.thing_to_do(:bad_first_arg, bad_function)
  end

  test "does not call the function if the key is wrong", context do
    assert {:error, _} =
             YourModule.thing_to_do(:bad_first_arg, context.bad_function)
  end
end
