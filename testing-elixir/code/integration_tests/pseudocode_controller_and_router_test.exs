#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule IntegrationTest do
  use ExUnit.Case

  test "POST /perform_action" do
    params = %{"some" => "params"}
    response = simulate_http_call("POST", "/perform_action", params)
    assert response.status == 200
    assert response.body == "OK"
  end
end
