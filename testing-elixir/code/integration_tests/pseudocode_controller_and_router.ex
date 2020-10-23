#---
# Excerpted from "Testing Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit http://www.pragmaticprogrammer.com/titles/lmelixir for more book information.
#---
defmodule MyApp.Router do
  route("POST", "/perform_action", {MyApp.Controller, :perform_action})
end

defmodule MyApp.Controller do
  def perform_action(connection, params) do
    parsed_params = parse_params(params)
    action_result = perform_action(parsed_params)
    response = build_response(action_result)
    send_response(connection, response)
  end
end
